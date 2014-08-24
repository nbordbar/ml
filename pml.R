setwd("C:/Users/nassim.bordbar/Documents/Coursera/ml")

library(caret)

#reading csv and replacing blank cells with NA
training <- read.csv("pml-training.csv",na.strings = c("","NA"))
head(training)
validation <- read.csv("pml-testing.csv",na.strings = c("","NA"))
head(validation)

#Removing columns with NA 
training <- training[,colSums(is.na(training)) == 0]
training <- training[,8:dim(training)[2]]
validation <- validation[,colSums(is.na(validation)) == 0]
validation <- validation[,8:dim(validation)[2]]

#Create training and testing dataset
inTrain <- createDataPartition(y=training$classe,p=0.75, list = FALSE)
testing <- training[-inTrain,]
training <- training[inTrain,]

#Application of random forests using caret package
tcr <- trainControl("repeatedcv", number=2, repeats=2, classProbs=TRUE, savePred=T) 
RFFit <- train(classe ~., data=training, method="rf", trControl=tcr)

#prediction for own testing dataset
testing$Prediction <- predict(RFFit, newdata=testing)
confusionMatrix(data=testing$Prediction, testing$classe)

#prediction algorithm applied to predict testing results
validation$Prediction <- predict(RFFit, newdata=validation)

#display results
validation$Prediction

