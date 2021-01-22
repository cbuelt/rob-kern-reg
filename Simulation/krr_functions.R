#This file includes the functions for preparing the standard KRR model


##############################Create model##########################

get_krr_model<-function(){
  
  #Create Parameters for krr model
  Parameters<-data.frame(parameter=c("sigma","lambda"),
                         class=rep("numeric",2),
                         label=c("Regularization","Sigma"))
  
  
  #Create grid for krr model
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
  
  
  #Create fit function for krr model
  Fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
    rbf<-kernlab::rbfdot(sigma=param$sigma)
    K<-kernlab::kernelMatrix(rbf,x)
    #Solve model using cholesky decomposition
    L_matrix <- t(chol(K + param$lambda * diag(dim(K)[1])))
    q <- solve(L_matrix,y)
    a <- solve(t(L_matrix),q)
    b <- mean(crossprod(K,a)-y)
    list("a"=a,"b"=b,"Kernel"=rbf,"x"=x)
  }
  
  
  #Create predict function for krr model
  Predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
    data_old<-as.matrix(modelFit$x)
    res<-kernlab::kernelMult(modelFit$Kernel,x=newdata,y=data_old,z=modelFit$a)+modelFit$b
    res
  }
  
  
  #Create krr model
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
