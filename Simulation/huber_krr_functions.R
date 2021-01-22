#This file includes the functions for preparing the Regression model
#with enhanced huber loss function

#Global variable to save parameters
global_param<<-list("m"=10,"epsilon"=0)
#Set method for global parameters
set_global_param<-function(m,epsilon){
  assign("global_param",list("m"=m,"epsilon"=epsilon),envir=.GlobalEnv)
}
#Get method for global parameters
get_global_param<-function(){
  return(global_param)
}



huber_group_func<-function(u,m,eps){
  x1<-u-eps
  x2<-u+eps
  res<-rep(3,length(u))
  res[x1>m]<-1
  res[x1>0 & x1<=m]<-2
  res[x2<0 & x2>= -m]<-4
  res[x2< -m]<-5
  return(res)
}


huber_value_func<-function(u,m,eps){
  x1<-u-eps
  x2<-u+eps
  res<-rep(0,length(u))
  res[x1>m]<-m*(2*x1[x1>m]-m)
  res[x1>0 & x1<=m]<-x1[x1>0 & x1<=m]^2
  res[x2<0 & x2>= -m]<-x2[x2<0 & x2>= -m]^2
  res[x2< -m]<- -m*(2*x2[x2< -m]+m)
  return(res)
}

#Custom huber-epsilon loss metric
huber_loss_metric<-function(data, lev=NULL,model=NULL){
  parameters<-get_global_param()
  m<-parameters$m
  eps<-parameters$epsilon
  residual<-data$obs-data$pred
  #Calculate Huber loss
  huber_loss<-huber_value_func(as.numeric(residual),m,eps)
  result_huber<-mean(huber_loss)
  
  #Calculate MAE
  result_mae<-mean(abs(residual))
  c("Huber_loss"=result_huber,"MAE"=result_mae)
}


#Create the whole Huber regression model
#Includes function for fitting, predicting and grid tuning
#Returns a model which can be used by the caret package
get_huber_krr_model<-function(){
  
  #Create Dataframe with parameters
  Parameters<-data.frame(parameter=c("sigma","lambda","m","epsilon"),
                         class=rep("numeric",4),
                         label=c("Sigma","Regularization","Linearization","Noise regulation"))
  
  
  #Create grid for parametertuning
  Grid <- function(x,y,length=NULL,search="grid"){
    if(search=="grid"){
      sigma<-c(0.1,0.5,2)
      lambda<-c(1)
      m<-c(3)
      eps<-c(0)
      grid<-expand.grid("sigma"=sigma,"lambda"=lambda,"m"=m,"epsilon"=eps)
    }else{
      sigma<-rnorm(length,mean=0,sd=1)
      lambda<-runif(length,min=0,max=1)
      m<-runif(length,min=1,max=5)
      eps<-runif(length,min=0,max=0.2)
      grid<-data.frame("sigma"=sigma,"lambda"=lambda,"m"=m,"epsilon"=eps)
    }
    
    grid
  }
  
  
  #Method to fit model to data
  Fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
    #Create kernel function
    rbf <- kernlab::rbfdot(sigma = param$sigma)
    #Create kernel matrix
    K<-kernlab::kernelMatrix(rbf,x)
    #Set boundaries for the algorithm
    max_iter=50
    eps=0.01
    #Prepare vectors and matrices to be used
    n<-dim(x)[1]
    energy<-numeric(max_iter)
    offset<-numeric(max_iter)
    a<-matrix(data = NA, ncol = n, nrow = max_iter)
    res<-matrix(data = NA, ncol = n, nrow = max_iter)
    group<-matrix(data = NA, ncol = n, nrow = max_iter)
    #First iteration of the algorithm
    a[1,]<-0
    offset[1] <- mean(y)
    residual<-(crossprod(K,a[1,])+offset[1])-y
    res[1,]<-residual
    
    #Calculate groups and energy for the first iteration
    group[1,]<-huber_group_func(as.numeric(residual),param$m,param$epsilon)
    energy[1]<-sum(huber_value_func(as.numeric(residual),param$m,param$epsilon))+
      (param$lambda*crossprod(a[1,],crossprod(K,a[1,])))
    
    
    #Loop of iterations
    for (iter in 2:max_iter){
      #Matrix I0
      S_length<-sum(group[iter-1,]==2)+sum(group[iter-1,]==4)
      
      #Create I_0 matrix
      if(S_length==0){
        I_0<-Matrix::sparseMatrix(1:1,1:1,x=0,dims=c(n,n))
      }else{
        I_0<-Matrix::sparseMatrix(1:S_length, 1:S_length, x = 1,dims=c(n,n))
      }
      
      #Vector e
      e<-plyr::mapvalues(group[iter-1,],from=c(1,2,3,4,5),to=c(param$m,param$epsilon,0,-(param$epsilon),-(param$m)),warn_missing = FALSE)
      #Vector q'
      q<- tcrossprod(-I_0,t(rep(offset[iter-1],n)-y))+e
      
      #Calculate Inverse of matrix M
      if(param$lambda==0){
        M<-as.matrix(tcrossprod(I_0,K))
        M_inv<-MASS::ginv(M)
      }else if(S_length==0){
        M_inv<-diag(1/param$lambda,nrow=n)
      }else if(S_length==n){
        K1<-K[1:S_length,1:S_length]+diag(param$lambda,S_length)
        M_inv<-base::solve(K1)
      }else{
        K1<-K[1:S_length,1:S_length]+diag(param$lambda,S_length)
        K1_inv<-base::solve(K1)
        K2<-(K[1:S_length,(S_length+1):n])
        upper<-cbind(K1_inv,(-1/param$lambda)*K1_inv%*%K2)
        lower<-cbind(matrix(0,nrow=n-S_length,ncol=S_length),diag(1/param$lambda,nrow=n-S_length))
        M_inv<-rbind(upper,lower)
      }
      
      
      #Value g
      g<-1/(crossprod(rep(1,n),tcrossprod(M_inv,t(rep(1,n)))))
      
      #New offset
      offset[iter]<-offset[iter-1]+g*crossprod(rep(1,n),tcrossprod(M_inv,t(q)))
      
      #New a
      a[iter,]<- as.numeric(tcrossprod(M_inv,t(q-(offset[iter]-offset[iter-1])*rep(1,n))))
      
      #Residuals
      residual<-(t(crossprod(K,a[iter,])+offset[iter])-y)
      res[iter,]<-residual

      #Groups
      group[iter,]<-huber_group_func(t(residual),param$m,param$epsilon)
      
      #Energy
      energy[iter]<-sum(huber_value_func(t(residual),param$m,param$epsilon))+
        (param$lambda*crossprod(a[iter,],crossprod(K,a[iter,])))
      
      #Delta energy
      delta_energy<-energy[iter]-energy[iter-1]
      #Intersection of groups
      group_intersect<-group[iter-1,]==group[iter,]
      if((abs(delta_energy)<eps)&&(TRUE %in% group_intersect)){
        break
      }
      #print(iter)
      #print(delta_energy)
    }

    #Set global parameters
    set_global_param(param$m,param$epsilon)
    
    #Get final parameters
    a_final<-a[iter,]
    offset_final<-offset[iter]
    
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
               library=c("kernlab","MASS","Matrix","plyr"),
               loop= NULL,
               prob=NULL,
               parameters=Parameters,
               grid=Grid,
               fit=Fit,
               predict=Predict)
  #Return Full Model
  return(Model)
  
}