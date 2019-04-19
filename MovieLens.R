#The goal is to predict movie ratings in the validation set. 
library(tidyverse)
library(randomForest)
library(rpart)
library(caret)
library(purrr)
library(chron)
library(stringr)
head(edx)

#Converting timestamp into date and time(!!Paste codes below)


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

#Creating Partitions
rating <- edx$rating
set.seed(1)
testIndex <- createDataPartition(rating, times = 1, p = 0.5, list = FALSE)
trainSet <- edx[-testIndex,]
testSet <- edx[testIndex,]

#Making sure that the two sets share users and movies. 
testSet <- testSet %>% 
          semi_join(trainSet, by = "movieID") %>% 
          semi_join(trainSet, by = "userId")

#releasing memory
rm(edx)
rm(testIndex)
rm(rating)


#Split genres into individual columns (this step is done after the set creation due to the lack of RAM)

