#Source functions
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("packages.R")
source("functions.R")
source("krr_functions.R")
source("huber_krr_functions.R")
source("ls_krr_functions.R")
source("kgard_functions.R")


####################################################Simulation for mean shift ###############################
#Import dataframe with optimal sigma values
parameters<-read_excel("../data/sigma_values.xlsx")

#Parameters
number_simulations <- 5
#Number of cores available for parallelization
n_cores <- detectCores()

#Create progress bar
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent)",
  total = number_simulations*dim(parameters)[1], clear = FALSE, width= 60)


#Create KRR Model
krr_model<-get_krr_model()
krr_fit <- trainControl(method="cv", number=5)

#Create Huber model
krr_huber_model<-get_huber_krr_model()
krr_huber_fit<-trainControl(method="cv", number=5, summaryFunction = huber_loss_metric)

#Create RKR model
weighted_krr_model<-get_ls_krr_model()

#Create KGARD model
kgard_model <- get_kgard_model()


#Create loop
results<-data.frame(matrix(data=NA,ncol=13,nrow=dim(parameters)[1],
                    dimnames=list(NULL, c("n-samples","p-dimension","Outlier percentage",
                                          "Outlier shift","Sigma","krr_mean_mse","krr_sd_mse",
                                          "huber_mean_mse","huber_sd_mse","weighted_krr_mean_mse",
                                          "weighted_krr_sd_mse","kgard_mean_mse","kgard_sd_mse"))))
#Vector for individual results
mean_vector_krr<-vector(length=number_simulations)
mean_vector_huber<-vector(length=number_simulations)
mean_vector_weighted<-vector(length=number_simulations)
mean_vector_kgard <- vector(length=number_simulations)

#Parallelization
cl <- makePSOCKcluster(n_cores)
clusterEvalQ(cl, {
  library(kernlab)
  library(MASS)
  library(Matrix)
})
clusterExport(cl, c("huber","get_global_param","set_global_param","huber_group_func","huber_value_func"))
registerDoParallel(cl)

#Measure total time
start.time <- Sys.time()

for(cnt in 1:dim(parameters)[1]){
  #Get parameters of current run
  param_actual<-parameters[cnt,]
  n <- as.numeric(param_actual["n-samples"])
  p <- as.numeric(param_actual["p-dimension"])
  gamma <- as.numeric(param_actual["Outlier percentage"])
  lambda_out <- as.numeric(param_actual["Outlier shift"])
  sigma <- as.numeric(param_actual["Sigma"])
  
  #Vector for lambda
  lambda<-c(seq(0.25,1.5,0.25),seq(2,5,0.5))
  
  #Create KRR grid
  grid_krr<-expand.grid("sigma"=sigma,"lambda"=lambda)
  
  #Create Kgard grid
  eps_kgard <- c(0.05,0.1)
  grid_kgard <- expand.grid("sigma"=sigma, "lambda"= lambda, "epsilon"=eps_kgard)
  
  #Create Huber grid
  m_huber<-c(seq(1.5,5,0.5))
  eps_huber<-c(0.005,0.01)
  grid_huber<-expand.grid("sigma"=sigma,"lambda"=lambda,"m"=m_huber,"epsilon"=eps_huber)

  
  #Second loop repeats each simulation several times for more robust results
  for (i in 1:number_simulations){
    #Get train-test-data
    data<-generate_dataset(n = n, p = p, outlier_perc =  gamma, lambda = lambda_out)
    training_data<-data[["training_data"]]
    testing_data<-data[["testing_data"]]
    

    ##############################Train models#####################
    #Train KRR model
    Trained_krr_model <- train(y ~ .,data=training_data,
                           method=krr_model,
                           trControl=krr_fit,
                           metric="MAE",
                           maximize=FALSE,
                           tuneGrid=grid_krr)
    
    #Train Huber model
    Trained_huber_model <- train(y ~ .,data=training_data,
                           method=krr_huber_model,
                           trControl=krr_huber_fit,
                           metric="MAE",
                           maximize=FALSE,
                           tuneGrid=grid_huber)
    
    #Train RKR model
    Trained_weighted_model <- train(y ~ .,data=training_data,
                               method=weighted_krr_model,
                               trControl=krr_fit,
                               metric="MAE",
                               maximize=FALSE,
                               tuneGrid=grid_krr)
    
    #Train Kgard model
    Trained_kgard_model <- train(y ~ .,data=training_data,
                                    method=kgard_model,
                                    trControl=krr_fit,
                                    metric="MAE",
                                    maximize=FALSE,
                                    tuneGrid=grid_kgard)
    
    ###################Predict#####################
    #KRR predict
    krr_predict <- predict(Trained_krr_model, testing_data)
    
    #Huber predict
    huber_predict <- predict(Trained_huber_model, testing_data)
    
    #Weighted krr predict
    weighted_krr_predict <- predict(Trained_weighted_model, testing_data)
    
    #Kgard predict
    kgard_predict <- predict(Trained_kgard_model, testing_data)
    
    #Calculate MSE
    mse_krr<- MSE(krr_predict,testing_data$y)
    mse_huber <- MSE(huber_predict,testing_data$y)
    mse_weighted <- MSE(weighted_krr_predict, testing_data$y)
    mse_kgard <- MSE(kgard_predict, testing_data$y)
    
    #Save results for one lambda
    mean_vector_krr[i]<-mse_krr
    mean_vector_huber[i]<-mse_huber
    mean_vector_weighted[i]<-mse_weighted
    mean_vector_kgard[i]<-mse_kgard
   
    #Create tick for progress bar
    pb$tick()

  }
  #Append results to dataframe
  results[cnt,]<-c(param_actual,mean(mean_vector_krr),sd(mean_vector_krr),mean(mean_vector_huber),
                   sd(mean_vector_huber),mean(mean_vector_weighted),sd(mean_vector_weighted),
                   mean(mean_vector_kgard),sd(mean_vector_kgard))
  #Save dataframe as excel file
  write_xlsx(results,path="Simulation_results.xlsx")
  
}
#Stop parallel
stopCluster(cl)
registerDoSEQ()
end.time <- Sys.time()



#By determining a plot index this function can plot the
plot<-1
plot_df<-results[plot:(plot+10),]
#Plot Error in dependece of lambda_outlier
ggplot(data=plot_df)+geom_line(aes(x=`Outlier.shift`,y=krr_mean_mse,color="KRR"))+
  geom_line(aes(x=`Outlier.shift`,y=huber_mean_mse,color="Huber"))+
  geom_line(aes(x=`Outlier.shift`,y=weighted_krr_mean_mse,color="Weighted KRR"))+
  geom_line(aes(x=`Outlier.shift`,y=kgard_mean_mse,color="KGARD"))+
  xlab("Outlier_shift")+ylab("MSE")


#Total time
end.time - start.time