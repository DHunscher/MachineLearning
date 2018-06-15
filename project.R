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

mags <- grep(".*magnet.*",colnames(training))
magnet <- training[,mags]
magnet <- cbind(classe = training$classe,magnet)

# modrf <- train(classe~.,data=magnet,method = "rf", prox=T)
# modgbm <- train(classe~.,data=magnet,method = "gbm",verbose=F)
modlda <- train(classe~.,data=magnet,method = "lda")

# predrf <- predict(modrf,testing)
# predgbm <- predict(modgbm,testing)
predlda <- predict(modlda,testing)

# rf <- predrf==testing$classe
# gbm <- predgbm==testing$classe
lda <- predlda==testing$classe

# print("rf:")
# print(length(which(rf==TRUE))/nrow(rf))
# print("gbm:")
# print(length(which(gbm==TRUE))/nrow(gbm))
print("lda:")
print(length(which(lda==TRUE))/length(lda))
confusionMatrix(testing$classe,predlda)

# print(nrow(training))
# print(nrow(testing))
# 
# print("done")


