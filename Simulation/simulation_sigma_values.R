#Source functions
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("packages.R")
source("functions.R")
source("krr_functions.R")

#Get cpu cluster count for parallelization
n_cores <- detectCores()

#Create parameter grid for the whole simulation
n<-c(150,500)
p<-c(1,2,5)
gamma<-seq(from = 0.05, to = 0.3, by = 0.05)
number_simulations <- 3
lambda_outlier<-seq(from = 2, to = 4.5, by=0.25)

param_grid <- expand.grid("n-samples" = n, "p-dimension" = p, "Outlier percentage" = gamma,
                          "Outlier shift" = lambda_outlier)
#Sort Grid
param_grid<-param_grid[order(param_grid$`p-dimension`,param_grid$`n-samples`,
                             param_grid$`Outlier percentage`),]
rownames(param_grid)<-NULL


#Create KRR Model
krr_model<-get_krr_model()
#Cross validation with 5  folds and 2 repeats
krr_fit <- trainControl(method="repeatedcv", number = 5, repeats = 2)

#Create Sigma search grid
sigma_krr<-c(seq(from = 0.05, to = 1, by = 0.05), seq(from = 1.4, to = 10, by = 0.4),
             seq(from = 11, to = 30, by = 1))
#Value for lambda stays at 1
lambda_krr<-c(1)
grid_krr<-expand.grid("sigma"=sigma_krr,"lambda"=lambda_krr)

#Create vector for best sigma values
sigma_best<-numeric(number_simulations)

#Create progress bar
pb <- progress_bar$new(
  format = "  Progress [:bar] :percent eta: :eta",
  total = number_simulations*20, clear = FALSE, width= 60)

#Create dataframe for results
results<-param_grid
results$Sigma<-NA

#Start loop over the whole parameter grid
for(x in 1:dim(param_grid)[1]){
  #Get parameters of current run
  param_actual<-param_grid[x,]
  n <- as.numeric(param_actual["n-samples"])
  p <- as.numeric(param_actual["p-dimension"])
  gamma <- as.numeric(param_actual["Outlier percentage"])
  lambda_out <- as.numeric(param_actual["Outlier shift"])

  #Start parallelization
  cl <- makePSOCKcluster(n_cores)
  clusterEvalQ(cl, {
    library(kernlab)
    library(MASS)
  })
  registerDoParallel(cl)
  
  #Repeat simulation on each parameter combination several times
  for (i in 1:number_simulations){
    #Create tick for progress bar
    pb$tick()
    #Get train-test-data
    data<-generate_dataset(n,p,gamma,lambda_out)
    training_data<-data[["training_data"]]
    testing_data<-data[["testing_data"]]
    
    #Train KRR model
    Trained_krr_model <- train(y ~ .,data=training_data,
                           method=krr_model,
                           trControl=krr_fit,
                           metric="MAE",
                           tuneGrid=grid_krr)
    #KRR predict
    krr_predict <- predict(Trained_krr_model, testing_data)
    #Best Sigma
    sigma_best[i]<-Trained_krr_model[["bestTune"]][["sigma"]]
  }
  #Stop parallel
  stopCluster(cl)
  registerDoSEQ()
  
  #Get mode of best sigma values or mean if mode does not exist
  uniqv <- unique(sigma_best)
  mode<-uniqv[which.max(tabulate(match(sigma_best, uniqv)))]
  if(length(which(sigma_best==mode))<=1){
    sigma<-round(mean(sigma_best),2)
  }else{
    sigma<-mode
  }
  #Write final results to excel file
  results[x,]<-c(as.numeric(param_actual),sigma)
  write_xlsx(results,path="Sigma_results.xlsx")
}