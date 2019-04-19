#Load packages needed
library(tidyverse)
library(caret)
library(purrr)
library(randomForest)

#Create smaller dataset for model testing
set.seed(1)
tinyTrain <- sample_n(trainSet, 100000, replace = FALSE)
tinyTest <- sample_n(testSet, 100000, replace = FALSE)

#Eliminating "movieID" 
tinyTrain <- within(tinyTrain, rm(movieId))
tinyTest <- within(tinyTest, rm(movieId))


#Create dataframe for predictors. 
predictors <- within(tinyTrain, rm(rating))


#k nearest neighbors
control <- trainControl(method = "cv", number = 2, p = .9)
pred <- tinyTrain[, 2]
train_knn <- train(predictors, pred,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7,9,11,13,15,17,19,21,23,25)),
                   trControl = control)
train_knn$bestTune
pred <- as.factor(pred)
fit_knn <- knn3(predictors, pred, k = train_knn$bestTune)
testPredictors <- within(tinyTest, rm(rating))
y_hat_knn <- predict(fit_knn,
                     testPredictors,
                     type = "class")
testPred <- tinyTest[,2]
cm <- confusionMatrix(y_hat_knn, factor(testPred))
cm$overall["Accuracy"]
RMSE(testPred, c(y_hat_knn))

#Tree 5-fold cross validation
library(Rborist)
control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(2,5), predFixed = c(5,15,20))
pred <- tinyTrain[, 2]

train_rf <- train(predictors,
                  pred,
                  method = "Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid, 
                  nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(predictors, pred, 
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)
testPredictors <- within(tinyTest, rm(rating))
testPred <- tinyTest[,2]
pred_rf <- predict(fit_rf, testPredictors)
y_hat_rf <- as.factor(levels(pred)[predict(fit_rf, testPredictors)$yPred])
testPred <- as.factor(testPred)
cm_rborist <- confusionMatrix(y_hat_rf, testPred)
cm$overall["Accuracy"]
RMSE(as.numeric(testPred), as.numeric(y_hat_rf))

#Comparing knn and RF
p_max <- predict(fit_knn, testPredictors)
p_max <- apply(p_max, 1 , max)
ind <- which(y_hat_knn != testPred)
ind <- ind[order(p_max[ind],decreasing = TRUE)]

#Ensembling the two models (currently not working, need fix)
p_rf <- c(y_hat_rf)
p_knn <- c(y_hat_knn)
p <- (p_rf + p_knn)/2
p <- as.data.frame(p)
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, testPred)

#None of these models have good accuracy, so let's try matrix factorization. 



