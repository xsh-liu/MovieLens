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

#Deleting the "genres" column after converting all genres into dummy variables
edx <- within(edx, rm(genres))
edx <- within(edx, rm(title))

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
