library(chron)

#Transform timestamp into normal date/time format
edx <- edx %>% mutate(timestamp = as.POSIXct(edx$timestamp, origin = "1970-01-01", tz = "GMT"))

validation <- validation %>% mutate(timestamp = as.POSIXct(validation$timestamp, origin = "1970-01-01", tz = "GMT"))


#Splitting date and time
edx <- edx %>% separate(col = timestamp, into = c("date", "time"), sep = " ")

validation <- validation %>% separate(col = timestamp, into = c("date", "time"), sep = " ")

#Check the format of the two new columns
class(edx$date)
class(edx$date)

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

