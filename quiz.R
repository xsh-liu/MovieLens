#Q1
dim(edx)

#Q2
head(edx, 50)

zero <- edx %>% filter(rating == 0)
nrow(zero)

three <- edx %>% filter(rating == 3)
nrow(three)

#Q3
movieID <- unique(edx$movieId)

#Answer code
n_distinct(edx$movieId)

#Q4
n_distinct(edx$userId)

#Q5
qfive <- separate_rows(data = edx, genres, sep = "\\|")
drama <- qfive %>% filter(genres == "Drama")
comedy <- qfive %>% filter(genres == "Comedy") 
thriller <- qfive %>% filter(genres == "Thriller") 
romance <- qfive %>% filter(genres == "Romance") 
nrow(drama)
nrow(comedy)
nrow(thriller)
nrow(romance)

#from Edx Help
if(!require(RSQLite))
  install.packages("RSQLite", repos = "http://cran.us.r-project.org")
library(RSQLite)
sqlite <- dbDriver("SQLite")
conn <- dbConnect(sqlite, "capstone.db")
dbWriteTable(conn, "edx", edx, overwrite=TRUE)
rm(edx)

qfive <- dbGetQuery(conn, 'SELECT genres FROM edx')
qfive1 <- qfive %>% summarize(genres = genres)

#Q6
qsix <- edx %>% group_by(movieId) %>% tally() %>% arrange()
edx %>% filter(movieId == "296")

#Answer's Codes
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Q7 
qseven <- edx %>% group_by(rating) %>% tally() %>% arrange()
head(qseven)
qseven

#Answer's Code
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))  

#Q8
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()