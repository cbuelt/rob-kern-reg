#This file includes the functions for preparing the Regression model
#with reweighting

##############################Create model##########################
get_ls_krr_model<-function(){
  #Create Dataframe with parameters
  Parameters<-data.frame(parameter=c("sigma","lambda"),
                         class=rep("numeric",2),
                         label=c("Sigma","Regularization"))
  
  
  #Create grid for parametertuning
  Grid <- function(x,y,length=NULL,search="grid"){
    if(search=="grid"){
      sigma<-c(0.1)
      lambda<-c(1)
      grid<-expand.grid("sigma"=sigma,"lambda"=lambda)
    }else{
      sigma<-rnorm(length,mean=0,sd=1)
      lambda<-runif(length,min=0,max=1)
      grid<-data.frame("sigma"=sigma,"lambda"=lambda)
    }
    
    grid
  }
  
  
  #Method to fit model to data
  Fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
    #Create kernel function
    rbf <- kernlab::rbfdot(sigma = param$sigma)
    #Create kernel matrix
    K<-kernlab::kernelMatrix(rbf,x)
    #Initialize parameters for the model
    max_iter<-50   #Maximum number of iterations
    eps<-0.001      #Stopping criterion
    
    #Initialize parameters with regular KRR and cholesky decomposition
    #a<-crossprod(t(MASS::ginv(K+diag(param$lambda,dim(K)[1]))),y)
    L_matrix <- t(chol(K + param$lambda * diag(dim(K)[1])))
    q <- solve(L_matrix,y)
    a <- solve(t(L_matrix),q)
    b <- mean(crossprod(K,a)-y)
    residuals<-crossprod(K,a)+b-y
    
    #Loop
    for(rep in 1:max_iter){
      #Initialize weights using logistic function
      weights_r<-sqrt(tanh(residuals)/residuals)
      #Calculate yw and Kw
      y_w<-y*weights_r
      K_w<-K*as.numeric(weights_r)

      #Calculate new parameters a and b
      a<-crossprod(t(MASS::ginv(K_w+diag(param$lambda,dim(K_w)[1]))),y_w)
      b<-mean(crossprod(K,a)-y)
      #Calculate residuals
      residuals_prev<-residuals
      residuals<-crossprod(K,a)+b-y
      #Check if stopping criterion is fulfilled
      delta<-norm(residuals_prev-residuals,type="2")/norm(residuals_prev,type="2")
      if(delta<eps){
        break
      }
    }
    a_final<-a
    offset_final<-b
    
    list("Kernel"=rbf,"data"=x,"a"=a_final,"offset"=offset_final)
  }
  
  
  #Method to predict new data
  Predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
    data_old<-as.matrix(modelFit$data)
    result<-kernlab::kernelMult(modelFit$Kernel,x=newdata,y=data_old,z=modelFit$a)+modelFit$offset
    result
  }
  
  #Modeling
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

