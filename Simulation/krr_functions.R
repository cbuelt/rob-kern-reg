#' This file includes the functions for preparing the standard KRR mode
#' 
#' 
#' Returns the KRR model in a caret model format
#'
#' @return List of model parameters
get_krr_model<-function(){
  #Create Parameters for model
  Parameters<-data.frame(parameter=c("sigma","lambda"),
                         class=rep("numeric",2),
                         label=c("Regularization","Sigma"))
  
#' Create parameter grid for model
#'
#' @param x Features
#' @param y Target data
#' @param length Parameter not relevant
#' @param search Grid/random search
#'
#' @return Dataframe containing the parameter grid
  Grid <- function(x,y,length=NULL,search="grid"){
    if(search=="grid"){
      sigma<-seq(0.1,0.5)
      lambda<-seq(0,1)
      grid<-expand.grid("sigma"=sigma,"lambda"=lambda)
    }else{
      sigma<-rnorm(length,mean=0,sd=1)
      lambda<-runif(length,min=0,max=1)
      grid<-data.frame("sigma"=sigma,"lambda"=lambda)
    }
  
    grid
  }
  
#' Create fit function for model
#'
#' @param x Features
#' @param y Target data
#' @param wts 
#' @param param Parameters used for the training
#' Further parameters nor relevant, see caret documentation
#' @param lev 
#' @param last 
#' @param weights 
#' @param classProbs 
#' @param ... 
#'
#' @return Returns model parameters, the kernel function and the data used for training
  Fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
    #Calculate kernel matrix
    rbf<-kernlab::rbfdot(sigma=param$sigma)
    K<-kernlab::kernelMatrix(rbf,x)
    #Solve model using cholesky decomposition
    L_matrix <- t(chol(K + param$lambda * diag(dim(K)[1])))
    q <- solve(L_matrix,y)
    a <- solve(t(L_matrix),q)
    b <- mean(crossprod(K,a)-y)
    #Return parameters
    list("a"=a,"b"=b,"Kernel"=rbf,"x"=x)
  }
  
  
#' Create predict function for model
#'
#' @param modelFit The fitted model
#' @param newdata The data that should be predicted
#' Irrelevant parameters (see caret documentation)
#' @param preProc 
#' @param submodels 
#'
#' @return Returns the predictions for the new data
  Predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
    data_old<-as.matrix(modelFit$x)
    res<-kernlab::kernelMult(modelFit$Kernel,x=newdata,y=data_old,z=modelFit$a)+modelFit$b
    res
  }
  
  #Create final model
  Model<- list(type="Regression",
                  library=c("kernlab","MASS"),
                  loop= NULL,
                  prob=NULL,
                  parameters=Parameters,
                  grid=Grid,
                  fit=Fit,
                  predict=Predict)
  return(Model)
}
