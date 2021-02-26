
#Removing the objects and setting up a clean environment#
rm(list = ls())

#Installing packages and binding libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#Downloading the data

td = tempdir()
edx = tempfile(tmpdir=td, fileext=".rds")
validation = tempfile(tmpdir=td, fileext=".rds")
download.file("https://www.dropbox.com/s/nspymeso8rmmak1/edx.rds?dl=1", edx)
download.file("https://www.dropbox.com/s/x0s477b0kzxpl6i/validation.rds?dl=1", validation)
edx = readRDS(edx)
validation = readRDS(validation)
unlink(td)

#Checking for null values and removing the corresponding rows
any(is.na(edx))
sum(is.na(edx))
edx <- edx %>% na.omit()

any(is.na(validation))
sum(is.na(validation))
validation <- validation %>% na.omit()

#Describe data
nrow(edx)
nrow(validation)
names(edx)
head(edx)
str(edx)

#Removing the Title column as it is not used in the analysis to make light weight date
edx<-edx[,-5]
edx <- validation[,-5]

#Converting timestamp to weekday and adding a new column named day_of_week for time bias analysis#
edx <- edx %>% mutate(day_of_week = wday(as_datetime(timestamp), label = TRUE))
validation <- validation %>% mutate(day_of_week = wday(as_datetime(timestamp), label = TRUE))

# Creating Test data sets for cross validation during training phase for different bias evaluation#
#so that Validation set is used only at the end.#

set.seed(1)
movie_index <- createDataPartition(y = edx$rating, times = 1, p = 0.25, list = FALSE)
movieset <- edx[movie_index,]

# Make sure userId and movieId in Trial set are also in edx set
movieset <- movieset %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

user_index <- createDataPartition(y = edx$rating, times = 1, p = 0.25, list = FALSE)
userset <- edx[user_index,]

# Make sure userId and movieId in Trial set are also in edx set
userset <- userset %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

genres_index <- createDataPartition(y = edx$rating, times = 1, p = 0.25, list = FALSE)
genresset <- edx[genres_index,]

# Make sure userId, movieId, and genres in Trial set are also in edx set
genresset <- genresset %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId") %>%
  semi_join(edx, by = "genres")

time_index <- createDataPartition(y = edx$rating, times = 1, p = 0.25, list = FALSE)
timeset <- edx[time_index,]

# Make sure userId, movieId, and genres in Trial set are also in edx set
timeset <- timeset %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId") %>%
  semi_join(edx, by = "day_of_week")

high_boxplot_genres_rating <- edx %>% filter(genres %in% c("Drama", "Comedy", "Comedy|Romance", "Comedy|Drama", "Comedy|Drama|Romance", "Drama|Romance")) %>% select(genres, rating)
str(high_boxplot_genres_rating)
head(high_boxplot_genres_rating)
mean(high_boxplot_genres_rating$rating)
high_boxplot_genres_rating$genres <- factor(high_boxplot_genres_rating$genres, levels = c("Drama", "Comedy", "Comedy|Romance", "Comedy|Drama", "Comedy|Drama|Romance", "Drama|Romance"), labels = c("Dr", "Co", "CoRo", "CoDr", "CoDrRo", "DrRo"))

boxplot(rating ~ genres, data = high_boxplot_genres_rating, xlab = "Genre", ylab = "Rating", main = "Power Genres")


low_boxplot_genres_rating <- edx %>% filter(genres %in% c("Action|Drama|Horror|Sci-Fi", "Action|Romance|Western", "Adventure|Comedy|Drama|Fantasy|Mystery|Sci-Fi", "Adventure|Crime|Horror|Thriller", "Adventure|Fantasy|Film-Noir|Mystery|Sci-Fi", "Adventure|Horror|Romance|Sci-Fi")) %>% select(genres, rating)
str(low_boxplot_genres_rating)
head(low_boxplot_genres_rating)
mean(low_boxplot_genres_rating$rating)
low_boxplot_genres_rating$genres <- factor(low_boxplot_genres_rating$genres, levels = c("Action|Drama|Horror|Sci-Fi", "Action|Romance|Western", "Adventure|Comedy|Drama|Fantasy|Mystery|Sci-Fi", "Adventure|Crime|Horror|Thriller", "Adventure|Fantasy|Film-Noir|Mystery|Sci-Fi"), labels = c("ADHSF", "ARW", "ACDFMSF", "ACHT", "AFMNMSF"))

boxplot(rating ~ genres, data = low_boxplot_genres_rating, xlab = "Genre", ylab = "Rating", main = "Power Genres")
# Clear plots
#if(!is.null(dev.list())) dev.off()

#Defining RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


lambdas <- seq(mean(low_boxplot_genres_rating$rating), mean(high_boxplot_genres_rating$rating)+0.5, 0.01)

rm(list = c("high_boxplot_genres_rating", "low_boxplot_genres_rating"))

naive_rmses <- sapply(lambdas, function(l){
  return(RMSE(validation$rating,l))
})

qplot(lambdas, naive_rmses)

# Clear plots
#if(!is.null(dev.list())) dev.off()

mu <- lambdas[which.min(naive_rmses)]
mu

rmse_results <- tibble(method = "naive", RMSE = min(naive_rmses))

movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + movieset %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)

predicted_ratings <- predicted_ratings %>% replace_na(mu)

movie_rmse <- RMSE(predicted_ratings, movieset$rating)
movie_rmse
rmse_results <- rmse_results %>% add_row(method = "movie", RMSE = movie_rmse)

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- userset %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

predicted_ratings <- predicted_ratings %>% replace_na(mu)

user_rmse <- RMSE(predicted_ratings, userset$rating)
user_rmse
rmse_results <- rmse_results %>% add_row(method = "user", RMSE = user_rmse)

genres_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- genresset %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>% pull(pred)

predicted_ratings <- predicted_ratings %>% replace_na(mu)

genre_rmse <- RMSE(predicted_ratings, genresset$rating)
genre_rmse
rmse_results <- rmse_results %>% add_row(method = "genre", RMSE = genre_rmse)

edx %>% group_by(day_of_week) %>% summarize(total = n(), ave = mean(rating)) %>% ggplot(aes(x=day_of_week, y=ave, fill=day_of_week)) + geom_bar(stat="identity")

# Clear plots
#if(!is.null(dev.list())) dev.off()

time_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  group_by(day_of_week) %>%
  summarize(b_d = mean(rating - mu - b_i - b_u - b_g))

predicted_ratings <- timeset %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  left_join(time_avgs, by='day_of_week') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%pull(pred)

predicted_ratings <- predicted_ratings %>% replace_na(mu)

time_rmse <- RMSE(predicted_ratings, timeset$rating)
time_rmse
rmse_results <- rmse_results %>% add_row(method = "time", RMSE = time_rmse)

lambdas <- seq(1.0, 4.0, 0.25)
rmses <- sapply(lambdas, function(l){
  b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- edx %>%
    left_join(b_i, by='movieId') %>% left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  
  b_d <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by='genres') %>%
    group_by(day_of_week) %>%
    summarize(b_d = sum(rating - mu - b_i - b_u - b_g)/(n()+l))

  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by='genres') %>%
    left_join(b_d, by='day_of_week') %>%
    mutate(pred = mu + b_i + b_u + b_g + b_d) %>% pull(pred)
  
  predicted_ratings <- predicted_ratings %>% replace_na(mu)
  
  RMSE(predicted_ratings, validation$rating)  
  return(RMSE(predicted_ratings, validation$rating))  
})
rmse_results
rmses
rmse_results$method <- factor(rmse_results$method, levels = rmse_results$method[order(rmse_results$RMSE)])
rmse_results %>% ggplot(aes(x=method, y=RMSE)) + geom_point(colour = 'red', size = 2)