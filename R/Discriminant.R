# ----------------------------------------------------------------------
# Discriminant function
# ----------------------------------------------------------------------
#' Discriminant
#' 
#' Find the Discriminant Function that separates three different categories.
#'
#' @param X A data frame or a matrix where rows are observations and columns are features. If \code{type} is "train" this is training dataset, and if it is "predict" it is test dataset.
#' @param Y A vector with labels for each row in \code{data} if \code{type} is "train", and with labels for each row in \code{memory} if \code{type} is "predict".
#' @param RealData A data frame or a matrix where rows are observations and columns are features. If \code{type} is "train" this argument is not needed, and if it is "predict" it is a training dataset.
#' @param k Number of neighbors that the classifier should use. It has to be an odd number.
#' @param obj Whether the goal is to train the classifier or predict classes of new observations based on past ones. The value can be either "train" or "predict".
#' @return A list with following elements: predictedClasses, accuracy and errorCount.
#' @export
#' @import assertthat 
#' @import hclass
#' @examples
#' # create artificial dataset
#' inputsTest   <- matrix(rnorm(200), ncol=2)
#' inputsTrain  <- matrix(rnorm(200), ncol=2)
#' classesTrain <- c(rep(0, 50), rep(1, 50))
#' # get the kNN predictions for the test set
#' kNN_classifier(inputsTest, classesTrain, inputsTrain, k=15, obj="predict")


X<-animalsDf[,1:2]
Y<-animalsDf[,3]
points<-100
k<-5

test<- function(X,Y,k,points){
  #take the categories we are working with
  realcategories<-unique(Y)
  realcategories<-as.character(realcategories)
  
  #take the name of the X's
  realcolnames<-colnames(X)
  
  #creating the grid 
  xlen <- ylen <- points
  X1<-seq(min(X[,1]),max(X[,1]),len=xlen)
  X2<-seq(min(X[,2]),max(X[,2]),len=ylen)
  data<-matrix(0,(xlen*ylen),2)
  colnames(data)<-c(realcolnames[1],realcolnames[2])
  is<-1
  for (i in 1:xlen){
    for (j in 1: ylen){
      data[is,]<-c(X1[i],X2[j])
      is<-is+1
    }
  }
  ##getting the labels for grid points
  labels<- hclass::KNN.k(X=data,Y=Y,k=k,obj="predict",RealData=X[,1:2])
  dataLabel<-cbind(data,labels$predictedClasses)
  ##diferentiate between point or boundary
  Tpoints<-matrix(0,xlen*ylen,4)
  plus <- 0
  for (i in 1:xlen){
    for (j in 2:ylen){
      if(dataLabel[(j+plus),3] == dataLabel[(j+plus-1),3]){
        Tpoints[(j+plus),4]<-0
        Tpoints[(j+plus),1:3]<-as.numeric(dataLabel[(j+plus),1:3])
      }else{
        Tpoints[(j+plus),4]<-1
        Tpoints[(j+plus),1:3]<-as.numeric(dataLabel[(j+plus),1:3])
        
      }
    }
    plus <- plus + ylen 
  }
  
  Tpoints<-as.data.frame(Tpoints)
  
  ##Changing the animals to it's category name
  for(i in 1:nrow(Tpoints)){
    if(Tpoints[i,3]==1){
      Tpoints[i,3]<-realcategories[1]
    }
    if(Tpoints[i,3]==2){
      Tpoints[i,3] <- realcategories[2]
    }
    if(Tpoints[i,3]==3){
      Tpoints[i,3] <- realcategories[3]
    }
  }
  ##Changing the type of point to either Boundary or Point
  for(i in 1:nrow(Tpoints)){
    if(Tpoints[i,4]==1){
      Tpoints[i,4]<-"Boundary"
    }
    if(Tpoints[i,4]==0){
      Tpoints[i,4] <- "Point"
    }
  }
  colnames(Tpoints)<-c(realcolnames[1],realcolnames[2],"category","type")
  Tpoints<- Tpoints[Tpoints$category != 0, ]
  Bou.Points<-Tpoints[Tpoints$type == "Boundary",]
  Po.points<-Tpoints[Tpoints$type == "Point",]
  TrueData <- cbind(X,Y)
  colnames(TrueData)<-c(realcolnames[1],realcolnames[2],"category")


ggplot()+
  geom_point(aes(x=Bou.Points[,1],y=Bou.Points[,2]),data=Bou.Points, lwd=1)+
  geom_point(aes(x=Po.points[,1],y=Po.points[,2], colour=category),data=Po.points, alpha=1/4, pch=1, lwd=1)+
  geom_point(aes(x=TrueData[,1], y=TrueData[,2], colour=category), data=TrueData, pch=18, lwd=3)+
  xlab("Weight") +
  ylab("Height")
}



