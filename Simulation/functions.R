#' This file includes all the functions corresponding to
#' generating the data for the simulation, as well as general functions
#'
#'
#' Calculates the interval where data is generated,
#' depending on the dimension
#' 
#' @param p Dimension of the data
#' @return Lower and upper boundaries of the interval
get_interval <- function(p){
  if(p==1){
    lower<- -15
    upper<- 15
  }else{
    lower<- 0
    upper<- 1
  }
  return(c(lower,upper))
}
#' Applies the test function, specific to dimension p, to the input data
#'
#' @param x Input data
#' @param p Dimension of the input data
#'
#' @return The function value y
get_test_function <- function(x,p){
  if(p==1){
    #Sinc function
    y<-sin(x)/x
  }else if (p==2){
    #Spike function
    x_1 <- 10 * (x-1/3)
    x_2 <- 10 * (x-2/3)
    
    norm_1 <- sqrt(sum(x_1^2))
    norm_2 <- sqrt(sum(x_2^2))
    
    phi1 <- (2*pi)^(-1) * exp(-0.5*(norm_1^2))
    phi2 <- (2*pi)^(-1) * exp(-0.5*(norm_2^2))
    
    y <- (10^2)/2 * (phi1 + phi2)
  }else if (p==5){
    #Friedman function
    x1 <- x[1]
    x2 <- x[2]
    x3 <- x[3]
    x4 <- x[4]
    x5 <- x[5]
    term1 <- 10 * sin(pi*x1*x2)
    term2 <- 20 * (x3-0.5)^2
    term3 <- 10*x4
    term4 <- 5*x5
    y <- term1 + term2 + term3 + term4
  }else{
    y<-0
  }
  return(y)
}

#' Generates the whole dataset based on the given parameters
#' Includes training and testing set
#'
#' @param n The size of the data (n-observations)
#' @param p The dimension of the data (p-dimennsions)
#' @param outlier_perc The wanted percentage of outliers
#' @param lambda The factor by which the outliers are shifted
#' @param train_test_split The percentage for the train-test split
#'
#' @return Training and testing dataset
generate_dataset<-function(n,p,outlier_perc,lambda,train_test_split=0.75){
  #Set noise
  noise<-0.1
  
  #Get interval dependent on dimension/evaluation function
  interval<-get_interval(p)
  
  #Generate uniform distributed data
  X_sample <- replicate(p,runif(n, min = interval[1], max = interval[2]))
  y_sample <- apply(X = X_sample, MARGIN = 1, FUN = function(x) get_test_function(x,p))
  
  #Standardize sample
  y_sample <- (y_sample -mean(y_sample))/sd(y_sample)
  
  #Generate train-test split
  split<-createDataPartition(y=y_sample,p=train_test_split,list=FALSE)
  X_train<-X_sample[split,]
  y_train<-y_sample[split]
  X_test<-X_sample[-split,]
  y_test<-y_sample[-split]
  
  #Get shift of data, by multiplying lambda with the standard deviation
  lambda_sd<-lambda*sd(y_train)

  #Check if outliers are present
  if(outlier_perc!=0){
    #Create outlier split
    outlier_split<-createDataPartition(y=y_train,p=outlier_perc,list=FALSE)
    #Add noise to inlier samples
    y_train[-outlier_split]<-y_train[-outlier_split]+rnorm(length(y_train[-outlier_split]),mean=0,sd=noise)
    y_test <- y_test + rnorm(length(y_test),mean=0,sd=noise)
    #Add outlier noise in both directions
    length_outlier<-length(y_train[outlier_split])
    #Use binomial distribution to determine positive/negative shift
    pos<-rbinom(1,length_outlier,0.5)
    outlier_fact<-c(rnorm(pos,mean=lambda_sd,sd=5*noise),rnorm(length_outlier-pos,mean= -lambda_sd,sd=5*noise))
    y_train[outlier_split]<-y_train[outlier_split]+outlier_fact
  }else{
    #Add noise to samples
    y_train<-y_train+rnorm(length(y_train),mean=0,sd=noise)
    y_test <- y_test + rnorm(length(y_test),mean=0,sd=noise)
  }
  #Prepare final dataframe
  training_data<-data.frame("y"=y_train,"X"=X_train)
  testing_data<-data.frame("y"=y_test,"X"=X_test)
  data<-list("training_data"=training_data,"testing_data"=testing_data)
  return(data)
}