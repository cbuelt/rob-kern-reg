#Load packages
library(caret)
library(mlbench)
library(mvtnorm)
library(kernlab)
library(MASS)
library(MLmetrics)
library(Matrix)
library(plyr)
library(listdtr)
library(corrplot)
library(progress)
#Source functions
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("functions.R")
source("krr_functions.R")
source("huber_krr_functions.R")


####################################################Simulation for mean shift ###############################




#Parameters
n<-25
p<-5
gamma<-0.02
tt_split <- 0.75
noise <- 0.02
lambda_out <-3

#Create KRR Model
krr_model<-get_krr_model()
krr_fit <- trainControl(method="cv", number=5)

#Create KRR grid
sigma_krr<-seq(0.1,3.1,0.2)
lambda_krr<-seq(0,4,0.5)
grid_krr<-expand.grid("lambda"=lambda_krr,"sigma"=sigma_krr)


#Measure time
start_time <- Sys.time()

#Create progress bar
pb <- progress_bar$new(
  format = "  Progress [:bar] :percent eta: :eta",
  total = 100, clear = FALSE, width= 60)

#Get train-test-data
data<-generate_dataset(n,p,gamma,tt_split,noise,lambda_out)
training_data<-data[["training_data"]]
testing_data<-data[["testing_data"]]


#Train KRR model
Trained_krr_model <- train(y ~ .,data=training_data,
                       method=krr_model,
                       trControl=krr_fit,
                       metric="RMSE",
                       maximize=FALSE,
                       tuneGrid=grid_krr)
#KRR predict
krr_predict <- predict(Trained_krr_model, testing_data)


#Get results
results<-Trained_krr_model[["results"]]
results<-results[order(results$lambda),]

#Correlation
len_lambda<-length(lambda_krr)
len_sigma<-length(sigma_krr)
correlation<-matrix(NA,nrow=len_sigma,ncol=len_lambda)

cnt1<-1
cnt2<-len_sigma
for(i in 1:len_lambda){
  pb$tick()
  print(results[cnt1:cnt2,][["RMSE"]])
  correlation[,i]<-results[cnt1:cnt2,][["RMSE"]]
  cnt1<-cnt1+len_sigma
  cnt2<-cnt2+len_sigma
}
corr_data<-data.frame(correlation)
colnames(corr_data)<-lambda_krr
corrplot(cor(corr_data),title = "Correlation for sigma=seq(0.1,3.1,0.2)",
         method = "number",type="lower")



#Plot train test
plot(X_train,y_train)
plot(X_test,y_test)
points(X_test,krr_predict,col="red")
MSE(krr_predict,y_test)

#Plot Error in dependece of lambda_outlier
ggplot(data=result_df)+geom_line(aes(x=lambda,y=krr_mean_mse,color="KRR"))

end_time - start_time



