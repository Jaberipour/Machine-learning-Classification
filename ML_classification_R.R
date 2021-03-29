library("caret")
library(lattice)
library(ggplot2)
library(e1071)
library(randomForest)
library(rpart)
library(kknn)
library(earth)

set.seed(100)

file <- "transfusion.csv"
Data_Blood <- read.csv(file)

inTrain <- createDataPartition(Data_Blood$Target, p = 0.85, list = FALSE)

TrainData <- Data_Blood[inTrain,-ncol(Data_Blood)]
TestData  <- Data_Blood[-inTrain,-ncol(Data_Blood)]

TrainClasses<-Data_Blood[inTrain,ncol(Data_Blood)]
TestClasses<-Data_Blood[-inTrain,ncol(Data_Blood)]

preProc <- preProcess(TrainData,method= c("center", "scale"))

train_Transformed <- predict(preProc,TrainData)
test_Transformed <- predict(preProc,TestData)


fit_control <- trainControl(method="repeatedcv", number=10, repeats=5,classProbs = TRUE,summaryFunction = twoClassSummary)
model <- train(train_Transformed, TrainClasses,, method = "knn",trControl = fit_control)

Best_Fit<-model$finalModel
print(Best_Fit)
test_pred <- predict(Best_Fit,  test_Transformed)
df <- as.data.frame(test_pred)
zero_class<-df[,1]
test_pred<-ifelse(zero_class >= 0.5, 'NO', 'Yes')
out<-confusionMatrix(as.factor(test_pred),as.factor(TestClasses))
print(out)

