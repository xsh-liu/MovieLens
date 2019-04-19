if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Smaller sets
edx <- edx[c(1:300000, 3000000:3300000, 6000000:6300000),]
validation <- validation[c(1:33333, 300000:333333, 600000:633333),]

#Load all the packages needed. 
library(tidyverse)
library(randomForest)
library(rpart)
library(caret)
library(purrr)
library(chron)
library(stringr)


#Transform timestamp into normal date/time format
edx <- edx %>% mutate(timestamp = as.POSIXct(edx$timestamp, origin = "1970-01-01", tz = "GMT"))

validation <- validation %>% mutate(timestamp = as.POSIXct(validation$timestamp, origin = "1970-01-01", tz = "GMT"))


#Splitting date and time
edx <- edx %>% separate(col = timestamp, into = c("date", "time"), sep = " ")

validation <- validation %>% separate(col = timestamp, into = c("date", "time"), sep = " ")

#Check the format of the two new columns
class(edx$date)
class(validation$date)

#Separate year, month, and date from the "date" column
datePrep <- edx$date
ratingYear <- str_sub(string = datePrep, start = 1, end = 4)
ratingMonth <- str_sub(string = datePrep, start = 6, end = 7)
ratingDay <- str_sub(string = datePrep, start = 9, end = 10)
edx <- cbind(edx, ratingYear)
edx <- cbind(edx, ratingMonth)
edx <- cbind(edx, ratingDay)

datePrep <- validation$date
ratingYear <- str_sub(string = datePrep, start = 1, end = 4)
ratingMonth <- str_sub(string = datePrep, start = 6, end = 7)
ratingDay <- str_sub(string = datePrep, start = 9, end = 10)
validation <- cbind(validation, ratingYear)
validation <- cbind(validation, ratingMonth)
validation <- cbind(validation, ratingDay)

#Remove the original "date" column to slim down the set. 
edx <- edx[, -4]
validation <- validation[, -4]

#Separate hour, minute, and second from the "time" column.
timePrep <- edx$time
ratingHour <- str_sub(string = timePrep, start = 1, end = 2)
ratingMin <- str_sub(string = timePrep, start = 4, end = 5)
ratingSec <- str_sub(string = timePrep, start = 7, end = 8)
edx <- cbind(edx, ratingHour)
edx <- cbind(edx, ratingMin)
edx <- cbind(edx, ratingSec)

timePrep <- validation$time
ratingHour <- str_sub(string = timePrep, start = 1, end = 2)
ratingMin <- str_sub(string = timePrep, start = 4, end = 5)
ratingSec <- str_sub(string = timePrep, start = 7, end = 8)
validation <- cbind(validation, ratingHour)
validation <- cbind(validation, ratingMin)
validation <- cbind(validation, ratingSec)

#Remove the original "time" column to slim down the set. 
edx <- edx[, -4]
validation <- validation[, -4]

#Remove all the values created during this process. 
rm(datePrep)
rm(ratingDay)
rm(ratingHour)
rm(ratingMin)
rm(ratingMonth)
rm(ratingSec)
rm(ratingYear)
rm(timePrep)

#Create a column of release year. 
releasePrep <- edx$title
releaseYear <- str_sub(string = releasePrep, start = -5, end = -2)
edx <- cbind(edx, releaseYear)

releasePrep <- validation$title
releaseYear <- str_sub(string = releasePrep, start = -5, end = -2)
validation <- cbind(validation, releaseYear)

#Remove the values no longer used. 
rm(releasePrep)
rm(releaseYear)

library(dplyr)
library(tidyr)

#Separate genres under the same title 
edx <- separate_rows(data = edx, genres, sep = "\\|", convert = FALSE)

#Transform character values into dummy variables
edx <- edx %>% mutate(Drama = ifelse(genres == "Drama", 1, 0)) %>%
  mutate(Crime = ifelse(genres == "Crime", 1, 0)) %>%
  mutate(Action = ifelse(genres == "Action", 1, 0)) %>%
  mutate(Adventure = ifelse(genres == "Adventure", 1, 0)) %>%
  mutate(Sci_Fi = ifelse(genres == "Sci-Fi", 1, 0)) %>%
  mutate(Thriller = ifelse(genres == "Thriller", 1, 0)) %>%
  mutate(Comedy = ifelse(genres == "Comedy", 1, 0)) %>%
  mutate(Mystery = ifelse(genres == "Mystery", 1, 0)) %>%
  mutate(Romance = ifelse(genres == "Romance", 1, 0)) %>%
  mutate(Animation = ifelse(genres == "Animation", 1, 0)) %>%
  mutate(Children = ifelse(genres == "Children", 1, 0)) %>%
  mutate(Fantasy = ifelse(genres == "Fantasy", 1, 0)) %>%
  mutate(War = ifelse(genres == "War", 1, 0)) %>%
  mutate(Horror = ifelse(genres == "Horror", 1, 0)) %>%
  mutate(Musical = ifelse(genres == "Musical", 1, 0)) %>%
  mutate(Western = ifelse(genres == "Western", 1, 0)) %>%
  mutate(Film_Noir = ifelse(genres == "Film-Noir", 1, 0)) %>%
  mutate(Documentary = ifelse(genres == "Documentary", 1, 0)) %>%
  mutate(IMAX = ifelse(genres == "IMAX", 1, 0)) %>%
  mutate(no_genres_listed = ifelse(genres == "(no genres listed)", 1, 0))

#Deleting the "genres" column after converting all genres into dummy variables and title column (assume movieId would just do the same). 
edx <- within(edx, rm(genres))
edx <- within(edx, rm(title))

#Creating Partitions
rating <- edx$rating
set.seed(1)
testIndex <- createDataPartition(rating, times = 1, p = 0.5, list = FALSE)
trainSet <- edx[-testIndex,]
testSet <- edx[testIndex,]

#Making sure that the two sets share users and movies. 
testSet <- testSet %>% 
  semi_join(trainSet, by = "movieId") %>% 
  semi_join(trainSet, by = "userId")

#releasing memory
rm(edx)
rm(testIndex)
rm(rating)

#Applying the same method to the validation set
validation <- separate_rows(data = validation, genres, sep = "\\|", convert = FALSE)

validation <- validation %>% mutate(Drama = ifelse(genres == "Drama", 1, 0)) %>%
  mutate(Crime = ifelse(genres == "Crime", 1, 0)) %>%
  mutate(Action = ifelse(genres == "Action", 1, 0)) %>%
  mutate(Adventure = ifelse(genres == "Adventure", 1, 0)) %>%
  mutate(Sci_Fi = ifelse(genres == "Sci-Fi", 1, 0)) %>%
  mutate(Thriller = ifelse(genres == "Thriller", 1, 0)) %>%
  mutate(Comedy = ifelse(genres == "Comedy", 1, 0)) %>%
  mutate(Mystery = ifelse(genres == "Mystery", 1, 0)) %>%
  mutate(Romance = ifelse(genres == "Romance", 1, 0)) %>%
  mutate(Animation = ifelse(genres == "Animation", 1, 0)) %>%
  mutate(Children = ifelse(genres == "Children", 1, 0)) %>%
  mutate(Fantasy = ifelse(genres == "Fantasy", 1, 0)) %>%
  mutate(War = ifelse(genres == "War", 1, 0)) %>%
  mutate(Horror = ifelse(genres == "Horror", 1, 0)) %>%
  mutate(Musical = ifelse(genres == "Musical", 1, 0)) %>%
  mutate(Western = ifelse(genres == "Western", 1, 0)) %>%
  mutate(Film_Noir = ifelse(genres == "Film-Noir", 1, 0)) %>%
  mutate(Documentary = ifelse(genres == "Documentary", 1, 0)) %>%
  mutate(IMAX = ifelse(genres == "IMAX", 1, 0)) %>%
  mutate(no_genres_listed = ifelse(genres == "(no genres listed)", 1, 0))


validation <- within(validation, rm(genres))
validation <- within(validation, rm(title))

#Create smaller dataset for model testing
set.seed(1)
tinyTrain <- trainSet[c(1:33333, 300000:333333, 600000:633334),]
tinyTest <- testSet[c(1:33333, 300000:333333, 600000:633334),]

#Remove the original train and test sets. 
rm(trainSet)
rm(testSet)

#Eliminating "movieID" only for predictive modeling (causing RAM issue) 
trainMovieId <- tinyTrain$movieId
testMovieId <- tinyTest$movieId
tinyTrain <- within(tinyTrain, rm(movieId))
tinyTest <- within(tinyTest, rm(movieId))

#Create dataframe for predictors. 
predictors <- within(tinyTrain, rm(rating))
#k nearest neighbors
control <- trainControl(method = "cv", number = 3, p = .8)
pred <- tinyTrain[, 2]
train_knn <- train(predictors, pred,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(15,25,2)),
                   trControl = control)
train_knn$bestTune
pred <- as.factor(pred)
fit_knn <- knn3(predictors, pred, k = train_knn$bestTune)
testPredictors <- within(tinyTest, rm(rating))
y_hat_knn <- predict(fit_knn,
                     testPredictors,
                     type = "class")
testPred <- tinyTest[,2]
cm_knn <- confusionMatrix(as.factor(y_hat_knn), as.factor(testPred))
rmse_knn <- RMSE(as.numeric(testPred), as.numeric(y_hat_knn))
cm_knn
rmse_knn

library(Rborist)
control <- trainControl(method = "cv", number = 3, p = 0.8)
grid <- expand.grid(minNode = c(2,5), predFixed = c(15,20,25))
pred <- as.factor(tinyTrain[, 2])

train_rf <- train(predictors,
                  pred,
                  method = "Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSample = 5000)
train_rf$bestTune

fit_rf <- Rborist(predictors, pred, 
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

testPredictors <- within(tinyTest, rm(rating))
testPred <- tinyTest[,2]
pred_rf <- predict(fit_rf, testPredictors)
y_hat_rf <- as.factor(levels(pred)[predict(fit_rf, testPredictors)$yPred])
testPred <- as.factor(testPred)
cm_rf <- confusionMatrix(y_hat_rf, testPred)
rmse_rf <- RMSE(as.numeric(testPred), as.numeric(y_hat_rf))
cm_rf
rmse_rf

#Using Principal Component Analysis to shrink down the number of predictors

pca <- prcomp(tinyTrain[,c(10:29)], center = FALSE, scale. = FALSE)
pcaTest <- prcomp(tinyTest[,c(10:29)], center = FALSE, scale. = FALSE)

summary(pca)

#Here we can see that the first 15 components capture ~98% of the variability. Let's use these 15 which represent genres and the rest of regular predictors. 

axes <- predict(pca, newdata = tinyTrain)
axesTest <- predict(pcaTest, newdata = tinyTest)

pcaData <- cbind(tinyTrain, axes)
pcaTestData <- cbind(tinyTest, axesTest)

#Create dataframe for predictors and predicted values. 
PCApredictors <- pcaData[,c(1,3:9,30:44)]
PCApred <- pcaData[,2]

PCAtestPredictors <- pcaTestData[,c(1,3:9,30:44)]
PCAtestPred <- pcaTestData[,2]

#Random Forest
PCAcontrol <- trainControl(method = "cv", number = 3, p = 0.8)
PCAgrid <- expand.grid(minNode = c(2,5), predFixed = c(10,15,20))
PCApred <- as.factor(tinyTrain[, 2])

PCAtrain_rf <- train(PCApredictors,
                     PCApred,
                     method = "Rborist",
                     nTree = 50,
                     trControl = PCAcontrol,
                     tuneGrid = PCAgrid,
                     nSample = 5000)
PCAtrain_rf$bestTune

PCAfit_rf <- Rborist(PCApredictors, PCApred, 
                     minNode = PCAtrain_rf$bestTune$minNode,
                     predFixed = PCAtrain_rf$bestTune$predFixed)

PCApred_rf <- predict(PCAfit_rf, PCAtestPredictors)
PCAy_hat_rf <- as.factor(levels(PCApred)[predict(PCAfit_rf, PCAtestPredictors)$yPred])
PCAtestPred <- as.factor(PCAtestPred)
PCAcm_rf <- confusionMatrix(PCAy_hat_rf, PCAtestPred)
PCArmse_rf <- RMSE(as.numeric(PCAtestPred), as.numeric(PCAy_hat_rf))
PCAcm_rf
PCArmse_rf

#Using the package "recommendarlab" 
library(recommenderlab)
library(dplyr)
library(tibble)

#Add "movieId" back into the sets.
tinyTrain <- add_column(tinyTrain, movieId = trainMovieId, .after = 1)
tinyTest <- add_column(tinyTest, movieId = testMovieId, .after = 1)

#Getting a recommender training set.
recomTrain <- tinyTrain

#Convert the dataframe to a rating matrix. 
recomTrain <- as(recomTrain, "realRatingMatrix")

#Create an evaluation scheme. 
e <- evaluationScheme(recomTrain, method = "cross-validation", train = 0.5, k = 3, given = -1)
e

#Evaluate different methods 
algos <- list(
  UBCF = list(name = "UBCF", param = NULL),
  IBCF = list(name = "IBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL),
  SVDF = list(name = "SVDF", param = NULL),
  AR = list(name = "AR", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL),
  RANDOM = list(name = "RANDOM", param = NULL),
  RERECOMMEND = list(name = "RERECOMMEND", param = NULL)
)

evlist <- evaluate(e, algos, type = "ratings")

plot(evlist, legend = "topright")
evResults <- avg(evlist)
evResults

#Create POPULAR recommender
r <- Recommender(getData(e, "train"), "POPULAR")
r

#Create predictions
p <- recommenderlab::predict(r, getData(e, "known"), type = "ratings")
p

#Calculate accuray
Accu <- as.data.frame(calcPredictionAccuracy(p, getData(e, "unknown"), byUser = TRUE)) %>% na.omit() %>%
  colMeans()
Accu

#Create hybrid recommender
rHy <- HybridRecommender(
  Recommender(getData(e, "train"), "UBCF"),
  Recommender(getData(e, "train"),"SVDF"),
  Recommender(getData(e, "train"),"POPULAR"),
  weights = NULL
)
rHy

#Create predictions
pHy <- recommenderlab::predict(rHy, getData(e, "known"), type = "ratings")
pHy

#Calculate accuray
AccuHy <- as.data.frame(calcPredictionAccuracy(pHy, getData(e, "unknown"), byUser = TRUE)) %>% na.omit() %>%colMeans()
AccuHy

#PCA with Recommenderlab
pca <- prcomp(tinyTrain[,c(11:30)], center = FALSE, scale. = FALSE)
axes <- predict(pca, newdata = tinyTrain)
pcaData <- cbind(tinyTrain, axes)
pcaSet <- pcaData[,c(1:10,31:45)]

#Convert the dataframe to a rating matrix. 
pcaSet <- as(pcaSet, "realRatingMatrix")

#Create an evaluation scheme. 
e <- evaluationScheme(pcaSet, method = "cross-validation", train = 0.5, k = 3, given = -1)
e

#Evaluate all methods.
algos <- list(
  UBCF = list(name = "UBCF", param = NULL),
  IBCF = list(name = "IBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL),
  SVDF = list(name = "SVDF", param = NULL),
  AR = list(name = "AR", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL),
  RANDOM = list(name = "RANDOM", param = NULL),
  RERECOMMEND = list(name = "RERECOMMEND", param = NULL)
)

PCAevlist <- evaluate(e, algos, type = "ratings")

plot(PCAevlist, legend = "topright")

PCAevResults <- avg(PCAevlist)
PCAevResults

#Use the top 3 models of different kinds to create a hybrid recommender. 
PCAHybrid <- HybridRecommender(
  Recommender(getData(e, "train"), "UBCF"),
  Recommender(getData(e, "train"),"SVDF"),
  Recommender(getData(e, "train"), "POPULAR"),
  weights = NULL
)

PCAHybrid

#Create predictions
PCAPred <- recommenderlab::predict(PCAHybrid, getData(e, "known"), type = "ratings")
PCAPred

#Calculate accuray
PCAAccu <- colMeans(as.data.frame(calcPredictionAccuracy(PCAPred, getData(e, "unknown"), byUser = TRUE)))
PCAAccu

#Releasing some memory for the larger "validation" set
rm(pca)
rm(axes)
rm(axesTest)
rm(pcaData)
rm(pcaSet)
rm(tinyTrain)
rm(tinyTest)
rm(train_knn)
rm(train_rf)
rm(y_hat_rf)
rm(y_hat_knn)
rm(fit_rf)
rm(fit_knn)

#PCA with Recommenderlab
pcaVali <- prcomp(validation[,c(11:30)], center = FALSE, scale. = FALSE)
axesVali <- predict(pcaVali, newdata = validation)
pcaDataVali <- cbind(validation, axesVali)
pcaSetVali <- pcaDataVali[,c(1:10,31:45)]


#Convert the dataframe to a rating matrix. 
pcaRecomSetVali <- as(pcaSetVali, "realRatingMatrix")

#Create an evaluation scheme. 
eRecomSetVali <- evaluationScheme(pcaRecomSetVali, method = "cross-validation", train = 0.5, k = 3, given = -1)
eRecomSetVali


#Create the model
rRecomSetVali <- HybridRecommender(
  Recommender(getData(eRecomSetVali, "train"), "UBCF"),
  Recommender(getData(eRecomSetVali, "train"),"SVDF"),
  Recommender(getData(eRecomSetVali, "train"), "POPULAR"),
  weights = NULL
)
rRecomSetVali

#Create predictions
pRecomSetVali <- recommenderlab::predict(rRecomSetVali, getData(eRecomSetVali, "known"), type = "ratings")
pRecomSetVali

#Calculate accuray
AccuVali <- colMeans(as.data.frame(calcPredictionAccuracy(pRecomSetVali, getData(eRecomSetVali, "unknown"), byUser = TRUE)))
AccuVali