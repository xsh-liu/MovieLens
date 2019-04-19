#Using Principal Component Analysis to shrink down the number of predictors

pca <- prcomp(tinyTrain[,c(11:30)], center = FALSE, scale. = FALSE)

summary(pca)

axes <- predict(pca, newdata = tinyTrain)

#Here we could see that the first 13 components captured ~95% of cumulative variation. 
pcaData <- cbind(tinyTrain, axes[,1:13])

tinyTrain <- pcaData[,c(1:10,31:43)]

set.seed(1)

tinyTrain <- sample_n(tinyTrain, 30000, replace = FALSE)

#Convert the dataframe to a rating matrix. 
tinyTrain <- as(tinyTrain, "realRatingMatrix")

#Create an evaluation scheme. 
e <- evaluationScheme(tinyTrain, method = "split", train = 0.9, k = 1, given = -1)
e

#Create user-based recommender
r <- HybridRecommender(
      Recommender(getData(e, "train"), "UBCF"),
      Recommender(getData(e, "train"),"SVDF"),
      Recommender(getData(e, "train"),"POPULAR"),
      weights = NULL
)
r

#Create predictions
p <- recommenderlab::predict(r, getData(e, "known"), type = "ratings")
p

#Calculate accuray
calcPredictionAccuracy(p, getData(e, "unknown"))
head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser = TRUE))


#Generate a list of methods, then run evaluation. 
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

evlist[[1]]
