# project for Machine Learning
library(UsingR)
library(caret)
library(dplyr)
library(ElemStatLearn)
library(tree)
library(pgmm)
library(rpart)
library(caret)
library(ISLR)
library(randomForest)
library(rpart.plot)
library(ggplot2)
library(datasets)
library(kernlab)
library(dummies)
library(dummies)
library(Hmisc)
library(AppliedPredictiveModeling)
library(MASS)
library(car)
library(splines)
library(olsrr)
library(rgl)
library(lmtest)
library(manipulate)
library(reshape2)
#library(galton)
#library(rlist)
library(vioplot)
library(aplpack)
library(tcltk)
library(graphics)
library(lubridate)
library(forecast)
library(e1071)
library(quantmod)
library(rlang)

set.seed(62433)

# should do http get to read file

pml.test  <- read.csv("~/pml-testing.csv")
pml.train <- read.csv("~/pml-training.csv")
nonNA_col <- c(grep("classe",colnames(pml.train)), 
               grep("_[xyz]$",colnames(pml.train)),
               grep("^roll_",colnames(pml.train)),
               grep("^pitch_",colnames(pml.train)),
               grep("^yaw_",colnames(pml.train)),
               grep("^total_accel_",colnames(pml.train)))

pml.work <- pml.train[,nonNA_col]
for (nm in colnames(pml.work)) {
  if (nm == "classe") { next }
  pml.work[,nm] == as.numeric(pml.work[,nm])
}
inTrain <- createDataPartition(pml.work$classe,p=0.7,list=FALSE)
training <- pml.work[inTrain,]
testing  <- pml.work[-inTrain,]



prc <- preProcess(training[,-1],method="pca",pcaComp = 12,center=T,scale=T)

ss <- sort(prc$std/sum(prc$std),decreasing = T)
View(ss)
