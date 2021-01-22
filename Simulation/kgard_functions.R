#' This file includes the functions for preparing the KGARD model
#' 
#' 
#' 
#' 
#' Returns the KRR model in a caret model format
#'
#' @return List of model parameters
get_kgard_model<-function(){
  #Create Dataframe with parameters
  Parameters<-data.frame(parameter=c("sigma","lambda","epsilon"),
                         class=rep("numeric",3),
                         label=c("Sigma","Regularization","Stopping criterion"))
  
  
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
      sigma<-c(0.1,0.5,2)
      lambda<-c(1)
      eps<-c(0.01)
      grid<-expand.grid("sigma"=sigma,"lambda"=lambda,"epsilon"=eps)
    }else{
      sigma<-rnorm(length,mean=0,sd=1)
      lambda<-runif(length,min=0,max=1)
      eps<-runif(length,min=0.01,max=0.2)
      grid<-data.frame("sigma"=sigma,"lambda"=lambda,"epsilon"=eps)
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
    #Create kernel function
    rbf <- kernlab::rbfdot(sigma = param$sigma)
    #Create kernel matrix
    K<-kernlab::kernelMatrix(rbf,x)
    #Set boundaries for the algorithm
    max_iter=150

    #Create data matrices X and B
    n<-dim(K)[1]
    X_matrix<-cbind(K,rep(1,n),diag(n))
    B_matrix<-rbind(cbind(diag(n),rep(0,n),diag(0,n)),
                    cbind(t(rep(0,n)),1,t(rep(0,n))),
                    cbind(diag(0,n),rep(0,n),diag(0,n)))
    
    #Create sets
    S<-seq(1,n+1)
    S_0_card<-length(S)
    SC<-seq(1,n)
    
    #Get subsets of data matrices
    X_set<-X_matrix[,S]
    B_set<-B_matrix[S,S]
    
    #Get first estimation vector using cholesky decomposition
    L_matrix<-t(Matrix::chol(crossprod(X_set) + param$lambda * B_set))
    q<-Matrix::solve(L_matrix,(crossprod(X_set,y)))
    z<-Matrix::solve(t(L_matrix),q)
    r <- y - tcrossprod(X_set,t(z))
    
    #Loop
    for(k in 1:max_iter){
      #Find argmax of the residuals with index in set SC
      max<-max(abs(r[SC]))
      j_1<-match(c(max),abs(r))
      i_1<-j_1+S_0_card
      
      #Update sets S and SC
      S <- append(S,i_1)
      SC <- SC[SC != j_1]
      
      #Update sets X and B
      X_set_prev<-X_set
      X_set<-X_matrix[,S]
      B_set<-B_matrix[S,S]
      
      #Find new estimator using cholesky decomposition
      e<-diag(n)[,j_1]
      d<-Matrix::solve(L_matrix,crossprod(X_set_prev,e))
      b<-sqrt(1-norm(d,type="2")^2)
      L_matrix<-rbind(cbind(L_matrix,rep(0,dim(L_matrix)[1])),
                      cbind(t(d),b))
      p<-Matrix::solve(L_matrix,crossprod(X_set,y))
      z <- Matrix::solve(t(L_matrix), p)
      
      #Calculate residuals and MSE of residuals
      r <- y - tcrossprod(X_set,t(z))
      delta<-mean(r^2)
      
      #If stopping criterion is met, stop the loop
      if(delta<param$epsilon){
        break
      }
    }
    #Extract final values
    a<-z[1:n]
    b<-z[n+1]
    list("Kernel"=rbf,"data"=x,"a"=a,"b"=b)
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
    data_old<-as.matrix(modelFit$data)
    result<-kernlab::kernelMult(modelFit$Kernel,x=newdata,y=data_old,z=modelFit$a)+modelFit$b
    result
  }
  
  
  #Create final model
  Model<- list(type="Regression",
               library=c("Matrix"),
               loop= NULL,
               prob=NULL,
               parameters=Parameters,
               grid=Grid,
               fit=Fit,
               predict=Predict)
  return(Model)
}