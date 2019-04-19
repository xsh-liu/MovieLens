#Using Principal Component Analysis to shrink down the number of predictors

pca <- prcomp(tinyTrain[,c(11:30)], center = FALSE, scale. = FALSE)
pcaTest <- prcomp(tinyTest[,c(11:30)], center = FALSE, scale. = FALSE)

summary(pca)

#Here we can see that the first 10 components capture ~86% of the variability

axes <- predict(pca, newdata = tinyTrain)
axesTest <- predict(pcaTest, newdata = tinyTest)

pcaData <- cbind(tinyTrain, axes)
pcaTestData <- cbind(tinyTest, axesTest)

#Eliminating MovieID
pcaData <- within(pcaData, rm(movieId))
pcaTestData <- within(pcaTestData, rm(movieId))

pcaData <- within(pcaData, rm(date))
pcaTestData <- within(pcaTestData, rm(date))

#Create dataframe for predictors and predicted values. 
predictors <- pcaData[,c(4:10,31:45)]
pred <- pcaData %>% select(rating)

#k nearest neighbors
control <- trainControl(method = "cv", number = 2, p = .9)
pred <- pcaData[, 3]
train_knn <- train(predictors, pred,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7,9,11,13)),
                   trControl = control)
train_knn$bestTune
pred <- as.factor(pred)
fit_knn <- knn3(predictors, pred, k = 13)
testPredictors <- pcaTestData[,c(4:10,31:45)]
y_hat_knn <- predict(fit_knn,
                     testPredictors,
                     type = "class")
testPred <- pcaTestData[,3]
cm <- confusionMatrix(y_hat_knn, factor(testPred))
cm$overall["Accuracy"]
RMSE(c(y_hat_knn), c(testPred))
