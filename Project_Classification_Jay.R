
library(caret)      #Library for data tuning and data preprocessing 
library(class)     #Library to use for KNN Algorithm installtion package install.packages("class)
library(RWeka)     #Package for C45 rule
library(party)
library(e1071)     #Package to use for Support Vector Machine
library(xlsx)
library(gmodels)



#Taking Input Data
InputData <- function() {
  InputData <- read.csv("Life_Expectancy_Dataset.csv") 
  InputData1 <- InputData[,c(3:6)]     #Taking Column 3 to 6 as they are the only one used for classification
  DataFrame <- data.frame(InputData1)
  return (DataFrame)
}

#Dividing the Input Dataset
divideDataset <- function(DataFrame,x){
  set.seed(x)
  return(sample(2,nrow(DataFrame),replace = TRUE, prob=c(0.8,0.2)))
}

#KNN algorithm 
myKNN <- function(TrainingData,TestData) {
  CLabel <- TrainingData[,4]
  return(knn(train = TrainingData[,1:3], test = TestData[,1:3],cl = CLabel, k=14.5,prob=TRUE))
}

#Support Vector Machine Algorithm
mySVM <- function(TrainingData) {
  return (svm(Continent ~., data = TrainingData,type = "C-classification",  kernel = "linear", cost = 10, Scale = FALSE))
}
mySVMPredict <- function(svmfit1,TestData) {
  predict(svmfit1, TestData[,1:4], type = "C-classification") 
}

#C45 Decision Tree Algorithm
myC45 <- function(TrainingData){
  return (J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 0),data = TrainingData))
}

#Ripper Descision Tree Algorithm
myRipper <- function(TrainingData){
  JRip(Continent~.,control = Weka_control(O = 0 , F = 7 , N = 1),data = TrainingData)
}

#Prediction Algorithm for test data for rule based classifier algorithm
myC45RipPredict <- function(m,TestData){
  return (predict(m,newdata = TestData1[,1:3]))
}

DataFrame <- InputData()     #Taking input data
#1

#Dividing Data into Training (80%) and Test (20%) set
CreatePartition <- divideDataset(DataFrame,32234)
TrainingData1 <- DataFrame[CreatePartition==1,1:4]
TestData1 <- DataFrame[CreatePartition==2,1:4]

#Implementation and Result of KNN
Test_Prediction <- myKNN(TrainingData1,TestData1)
confusionMatrix(Test_Prediction,TestData1[,4])     #Result and analysis of KNN

#Implementation and Result of Support Vector Machine
svmfit <- mySVM(TrainingData1)
p<-mySVMPredict(svmfit,TestData1)
confusionMatrix(p,TestData1[,4])                  #Result and analysis of SVM
#print(svmfit)
#plot(p)

#Implementation and result of C4.5 
m1 <- myC45(TrainingData1)
p1<- myC45RipPredict(m1,TestData1)
confusionMatrix(p1,TestData1[,4])                  #Result and analysis of C45

#Implementation and result of Ripper 
m2 <- myRipper(TrainingData1)
p2<- myC45RipPredict(m2,TestData1)
confusionMatrix(p2,TestData1[,4])                  #Result and analysis of Ripper


#2

#Dividing Data into Training (80%) and Test (20%) set
CreatePartition <- divideDataset(DataFrame,1234)
TrainingData1 <- DataFrame[CreatePartition==1,1:4]
TestData1 <- DataFrame[CreatePartition==2,1:4]

#Implementation and Result of KNN
Test_Prediction <- myKNN(TrainingData1,TestData1)
confusionMatrix(Test_Prediction,TestData1[,4])        #Result and analysis of KNN   

#Implementation and Result of Support Vector Machine
svmfit <- mySVM(TrainingData1)
p<-mySVMPredict(svmfit,TestData1)
confusionMatrix(p,TestData1[,4])                  #Result and analysis of SVM
#print(svmfit)
#plot(p)

#Implementation and Result of C4.5
m1 <- myC45(TrainingData1)
p1<- myC45RipPredict(m1,TestData1)
confusionMatrix(p1,TestData1[,4])                  #Result and analysis of C45

#Implementation and Result of Ripper
m2 <- myRipper(TrainingData1)
p2<- myC45RipPredict(m2,TestData1)
confusionMatrix(p2,TestData1[,4])                  #Result and analysis of Ripper

#3)Dividing Data into Training (80%) and Test (20%) set

CreatePartition <- divideDataset(DataFrame,2018)
TrainingData1 <- DataFrame[CreatePartition==1,1:4]
TestData1 <- DataFrame[CreatePartition==2,1:4]

#Implementation and Result of KNN
Test_Prediction <- myKNN(TrainingData1,TestData1)
confusionMatrix(Test_Prediction,TestData1[,4])        #Result and analysis of KNN

#Implementation and Result of Support Vector Machine
svmfit <- mySVM(TrainingData1)
p<-mySVMPredict(svmfit,TestData1)
confusionMatrix(p,TestData1[,4])                  #Result and analysis of SVM
#print(svmfit)
#plot(p)

#Implementation and Result of C4.5
m1 <- myC45(TrainingData1)
p1<- myC45RipPredict(m1,TestData1)
confusionMatrix(p1,TestData1[,4])                  #Result and analysis of C45

#Implementation and Result of Ripper
m2 <- myRipper(TrainingData1)
p2<- myC45RipPredict(m2,TestData1)
confusionMatrix(p2,TestData1[,4])                  #Result and analysis of Ripper



#4

#Dividing Data into Training (80%) and Test (20%) set
CreatePartition <- divideDataset(DataFrame,1264)
TrainingData1 <- DataFrame[CreatePartition==1,1:4]
TestData1 <- DataFrame[CreatePartition==2,1:4]

#Implementation and Result of KNN
Test_Prediction <- myKNN(TrainingData1,TestData1)
confusionMatrix(Test_Prediction,TestData1[,4])        #Result and analysis of KNN

#Implementation and Result of Support Vector Machine
svmfit <- mySVM(TrainingData1)
p<-mySVMPredict(svmfit,TestData1)
confusionMatrix(p,TestData1[,4])                  #Result and analysis of SVM
#print(svmfit)
#plot(p)

#Implementation and Result of C4.5
m1 <- myC45(TrainingData1)
p1<- myC45RipPredict(m1,TestData1)
confusionMatrix(p1,TestData1[,4])                  #Result and analysis of C45

#Implementation and Result of Ripper
m2 <- myRipper(TrainingData1)
p2<- myC45RipPredict(m2,TestData1)
confusionMatrix(p2,TestData1[,4])                  #Result and analysis of Ripper

#5

#Dividing Data into Training (80%) and Test (20%) set
CreatePartition <- divideDataset(DataFrame,1784)
TrainingData1 <- DataFrame[CreatePartition==1,1:4]
TestData1 <- DataFrame[CreatePartition==2,1:4]

#Implementation and Result of KNN
Test_Prediction <- myKNN(TrainingData1,TestData1)
confusionMatrix(Test_Prediction,TestData1[,4])        #Result and analysis of KNN

#Implementation and Result of Support Vector Machine
svmfit <- mySVM(TrainingData1)
p<-mySVMPredict(svmfit,TestData1)
(confusionMatrix(p,TestData1[,4]))               #Result and analysis of SVM
#print(svmfit)
#plot(p)

#Implementation and Result of C4.5
m1 <- myC45(TrainingData1)
p1<- myC45RipPredict(m1,TestData1)
confusionMatrix(p1,TestData1[,4])                  #Result and analysis of C45

#Implementation and Result of Ripper
m2 <- myRipper(TrainingData1)
p2<- myC45RipPredict(m2,TestData1)
confusionMatrix(p2,TestData1[,4])                  #Result and analysis of Ripper

