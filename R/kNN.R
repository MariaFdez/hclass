# ----------------------------------------------------------------------
# Simple kNN classifier
# ----------------------------------------------------------------------
#' kNN classifier 
#' 
#' Classify the input with a k nearest neighbors classifier, the distance is measure as a Euclidean Distance.
#'
#' @param X A data frame or a matrix where rows are observations and columns are features. If \code{type} is "train" this is training dataset, and if it is "predict" it is test dataset.
#' @param Y A vector with labels for each row in \code{data} if \code{type} is "train", and with labels for each row in \code{memory} if \code{type} is "predict".
#' @param RealData A data frame or a matrix where rows are observations and columns are features. If \code{type} is "train" this argument is not needed, and if it is "predict" it is a training dataset.
#' @param k Number of neighbors that the classifier should use. It has to be an odd number.
#' @param obj Whether the goal is to train the classifier or predict classes of new observations based on past ones. The value can be either "train" or "predict".
#' @return A list with following elements: predictedClasses, accuracy and errorCount.
#' @export
#' @import assertthat 
#' @examples
#' # create artificial dataset
#' inputsTest   <- matrix(rnorm(200), ncol=2)
#' inputsTrain  <- matrix(rnorm(200), ncol=2)
#' classesTrain <- c(rep(0, 50), rep(1, 50))
#' # get the kNN predictions for the test set
#' KNN.k(inputsTest, classesTrain, inputsTrain, k=15, obj="predict")


#Creating the function that trains or predicts the labels
KNN.k <- function (X,Y,k, obj="train",RealData=NULL){
  
  # test the inputs
  not_empty(X); not_empty(Y); 
  if (obj=="train") {
    assert_that(nrow(X)==length(Y))
  }
  is.string(obj); assert_that(obj %in% c("train", "predict"))
  is.count(k); 
  if (obj=="predict") {
    assert_that(not_empty(RealData) & 
                  ncol(RealData)==ncol(X) & 
                  nrow(RealData)==length(Y))
  }
  
  
  #how many observations
  noObs<-nrow(X)
  
  ##Path in case we wnt to train data
  if(obj=="train"){
    Distances<-matrix(0,noObs,noObs)
    for(i in 1:noObs){
      #probe of the current observation
      probe <- as.numeric(X[i,])
      probeExpanded <- matrix(rep(probe, each=noObs), nrow=noObs)
      #distances:
      Distances[i,]<-(rowSums((abs(X -probeExpanded))^2) )^(1/2)
    }
    #neighbors of each point
    neighbors<- matrix(0,nrow(Distances),nrow(Distances))
    neighbors <- apply(Distances, 2, order)
    ModeNeigh<- rep(NA,noObs)
    indexes <- t(neighbors[2:(k+1),]) #take only the k th neighbours
    indexes.labels <-matrix(0,noObs,k)
    #identifying the label that should have the observation
    for(i in 1:noObs){
      for(j in 1:k){
        ind<-indexes[i,j]
        indexes.labels[i,j] <-Y[ind] 
      }
    }
    predictedClass<-rep(NA,noObs)
    #choose the label of the mode category
    for (s in 1: noObs){
      predictedClass[s]<-as.numeric(names(table(indexes.labels[s,]))[which.max(table(indexes.labels[s,]))])
    }
    
  }
  
  #in case we want to predict with the real data values:
  if(obj=="predict"){
    noReal<-nrow(RealData)
    Distances<-matrix(0,noObs,noReal)
    for(i in 1:noObs){
      #probe of the current observation
      probe <- as.numeric(X[i,])
      probeExpanded <- matrix(rep(probe, each=noReal), nrow=noReal)
      #distances:
      Distances[i,]<-(rowSums((abs(RealData -probeExpanded))^2) )^(1/2)
    }
    #neighbors of each point
    neighbors<- matrix(0,noObs,nrow(Distances))
    neighbors <- apply(Distances, 1, order)
    
    ModeNeigh<- rep(NA,noObs)
    indexes <- t(neighbors[1:k,]) #take only the k th neighbours
    indexes.labels <-matrix(0,noObs,k)
    #identifying the label that should have the observation
    for(i in 1:noObs){
      for(j in 1:k){
        ind<-indexes[i,j]
        indexes.labels[i,j] <-Y[ind]
      }
    }
    predictedClass<-rep(NA,noObs)
    #choose the label of the mode category
    for (s in 1: noObs){
      predictedClass[s]<-as.numeric(names(table(indexes.labels[s,]))[which.max(table(indexes.labels[s,]))])
    }
    
  }
  
  # examine the performance, available only if training
  if (obj=="train") {
    errorCount <- table(predictedClass, Y)
    accuracy <- mean(predictedClass==as.numeric(Y))
  } else if (obj == "predict") {
    errorCount <- NA
    accuracy <- NA
  }
  
  # return the results
  return(list(predictedClasses=predictedClass,
              accuracy=accuracy,
              errorCount=errorCount))
}
