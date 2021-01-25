#Load packages
library(caret)
#library(mlbench)
#library(mvtnorm)
library(kernlab)
library(MASS)
library(MLmetrics)
library(Matrix)
library(plyr)
library(listdtr)
library(progress)
library(microbenchmark)
library(writexl)
#Source functions
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("functions.R")
source("krr_functions.R")
source("huber_krr_functions.R")
source("ls_krr_functions.R")
source("kgard_functions.R")


#Data parameters
n_seq<-c(seq(50,1000,50),seq(1200,2000,200),3000)
p<-1
gamma<-0.15
lambda<-3.5
cnt<-1

#Create progress bar
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent)",
  total = length(n_seq), clear = FALSE, width= 60)

#Create KRR Model
krr_model<-get_krr_model()

#Create KRR Huber model
krr_huber_model<-get_huber_krr_model()

#Create weighted model
weighted_krr_model<-get_ls_krr_model()

#Create kgard model
kgard_model <- get_kgard_model()

#Create training fit
train_fit <- trainControl(method="none")
huber_fit <- trainControl(method="none", summaryFunction = huber_loss_metric)

#################Create grids############
#Get value for sigma
sigma<-c(0.1)
#Vector for lambda
lambda_krr<-c(0.5)
lambda_krr2<-c(0.25)

#Create KRR grid
grid_krr<-expand.grid("sigma"=sigma,"lambda"=lambda_krr)
grid_weighted<-expand.grid("sigma"=sigma,"lambda"=lambda_krr2)

#Create kgard grid
eps_kgard<-c(0.05)
grid_kgard<-expand.grid("sigma"=sigma,"lambda"=lambda_krr2,"epsilon"=eps_kgard)

#Create Huber grid
m_huber<-c(2.5)
eps_huber<-c(0.005)
grid_huber<-expand.grid("sigma"=sigma,"lambda"=lambda_krr,"m"=m_huber,"epsilon"=eps_huber)

############################Prepare loop##########################


#Dataframe for results
results<-data.frame(matrix(data=NA,ncol=8,nrow=length(n_seq),
                           dimnames=list(NULL, c("p","n","outlier_perc","outlier_shift","krr",
                                                 "huber_krr","weighted_krr","KGARD"))))
###################Loop#####################
for(n in n_seq){
  #Determine number of repeats
  if(n<=1000){
    n_rep<-10
    n_rep_kgard<-5
  }else{
    n_rep<-3
    n_rep_kgard<-1
  }
  #Use microbenchmark to measure performance
  #First run for KRR, RKR, Huber
  res=microbenchmark(
    #Regular krr
    train(y ~ .,data=training_data,
          method=krr_model,
          trControl=train_fit,
          metric="MAE",
          maximize=FALSE,
          tuneGrid=grid_krr),
    #Huber krr
    train(y ~ .,data=training_data,
          method=krr_huber_model,
          trControl=huber_fit,
          metric="MAE",
          maximize=FALSE,
          tuneGrid=grid_huber),
    #Weighted krr
    train(y ~ .,data=training_data,
          method=weighted_krr_model,
          trControl=train_fit,
          metric="MAE",
          maximize=FALSE,
          tuneGrid=grid_weighted),
    times=n_rep,
    unit="s",
    setup = training_data<-generate_dataset(n = n, p = p,
                                            outlier_perc =  gamma, lambda = lambda,
                                            train_test_split = 1)[["training_data"]]
  )
  print("Summary")
  print(summary(res))
  
  #Use microbenchmark to measure performance
  #Second run for KGARD, because it takes way longer
  #so it gets repeated less times
  res2=microbenchmark(
    #Kgard
    train(y ~ .,data=training_data,
          method=kgard_model,
          trControl=train_fit,
          metric="MAE",
          maximize=FALSE,
          tuneGrid=grid_kgard),
    times=n_rep_kgard,
    unit="s",
    setup = training_data<-generate_dataset(n = n, p = p,
                                            outlier_perc =  gamma, lambda = lambda,
                                            train_test_split = 1)[["training_data"]]
  )
  
  #Save results in excel
  results[cnt,]<-c(p,n,gamma,lambda,summary(res)$median,summary(res2)$median)
  write_xlsx(results,path="runtime_results.xlsx")
  cnt<-cnt+1
  
  #Tick for progress bar
  pb$tick()
}
