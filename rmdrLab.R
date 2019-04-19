#Using the package "recommendarlab" 
library(recommenderlab)
library(dplyr)

#Getting a smaller training set for time efficiency. 
set.seed(1)
recomTrain <- sample_n(tinyTrain, 30000, replace = FALSE)

#Convert the dataframe to a rating matrix. 
recomTrain <- as(recomTrain, "realRatingMatrix")

#Create an evaluation scheme. 
e <- evaluationScheme(recomTrain, method = "cross-validation", k = 5, given = -1)
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
avg(evlist)

#Create POPULAR recommender
r <- Recommender(getData(e, "train"), "POPULAR")
r

#Create predictions
p <- recommenderlab::predict(r, getData(e, "known"), type = "ratings")
p

#Calculate accuray
Accu <- colMeans(as.data.frame(calcPredictionAccuracy(p, getData(e, "unknown"), byUser = TRUE)))


##
#Create hybrid recommender
rHy <- HybridRecommender(
  Recommender(getData(e, "train"), "UBCF"),
  Recommender(getData(e, "train"),"IBCF"),
  Recommender(getData(e, "train"),"POPULAR"),
  weights = NULL
)
rHy

#Create predictions
pHy <- recommenderlab::predict(rHy, getData(e, "known"), type = "ratings")
pHy

#Calculate accuray
AccuHy <- colMeans(as.data.frame(calcPredictionAccuracy(pHy, getData(e, "unknown"), byUser = TRUE)))

#Test the POPULAR model on the original validation set. 
##Convert the data frame into a rating matrix. 
recomVali <- as(validation, "realRatingMatrix")

#Create predictions
pVali <- recommenderlab::predict(r, recomVali, type = "ratings")
pVali

#Calculate accuray
AccuHy <- colMeans(as.data.frame(calcPredictionAccuracy(pVali, validation[,3], byUser = TRUE)))