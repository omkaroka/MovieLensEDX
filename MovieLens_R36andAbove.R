## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----download, message = FALSE, warning = FALSE-------------------------------------------------
 #Create test and validation sets
 #Create edx set, validation set, and submission file
options(warn=-1) #Suppress Warnings
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
#Loading Libraries:
library("stringr")
library("tidyverse")
library("caret")
library("anytime")
library("lubridate")
library("corrr")

# MovieLens 10M dataset:
 # https://grouplens.org/datasets/movielens/10m/
 # http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
 download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
 colnames(movies) <- c("movieId", "title", "genres")
										
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId), title = as.character(title), genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
 
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% semi_join(edx, by = "movieId") %>% semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)



## ----structure, warning = FALSE, message = FALSE------------------------------------------------
str(edx)


## ----data, warning = FALSE, message = FALSE-----------------------------------------------------
head(edx)


## ----formats, message = FALSE, echo = FALSE-----------------------------------------------------
edx$userId <- as.factor(edx$userId) # Convert `userId` to `factor`.
edx$movieId <- as.factor(edx$movieId) # Convert `movieId` to `factor`.
edx$genres <- as.factor(edx$genres) # Convert `genres` to `factor`.
edx$timestamp <- as.POSIXct(edx$timestamp, origin = "1970-01-01") # Convert `timestamp to `POSIXct`.

edx <- edx %>%
  mutate(title = str_trim(title)) %>%
  extract(title, c("title_tmp", "year"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(year = if_else(str_length(year) > 4,
                        as.integer(str_split(year, "-",
                                             simplify = T)[1]),
                        as.integer(year))) %>%
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  select(-title_tmp) %>% mutate(year_rate = year(timestamp))

edx$genres <- as.factor(edx$genres)


head(edx)


## ----rating_summary, message = FALSE, echo = FALSE----------------------------------------------
summary(edx$rating)


## ----Rating_Distribution, warning = FALSE, message = FALSE, echo = FALSE------------------------
edx %>% group_by(rating) %>% summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        mutate(Percentage = paste(round(n / sum(n) * 100, 2), "%", sep = "")) %>%
        knitr::kable(caption = "Rating_Distribution")


## ----plot_rating, warning = FALSE, message = FALSE, echo = FALSE, fig.height = 7, fig.width = 10, fig.align = "center"----
edx %>% ggplot(aes(rating)) +
            geom_histogram(fill = "darkblue") +
            labs(title = "Rating distribution",
                 x = "Rate",
                 y = "Frequency")


## ----unique_users, warning = FALSE, message = FALSE, echo = FALSE-------------------------------
edx %>% summarize(Unique_Users = n_distinct(userId), Unique_Movies = n_distinct(movieId),Unique_Genres = n_distinct(genres)) %>% knitr::kable(caption = "Unique_Numbers")



## ----year_release, message = FALSE, warning = FALSE, echo = FALSE-------------------------------
edx %>% ggplot(aes(year)) +
  geom_histogram(fill = "darkblue") +
  labs(title = "Release Year distribution",
       x = "Year",
       y = "Frequency")


## ----average_rating, message = FALSE, warning = FALSE, echo = FALSE-----------------------------
edx %>% group_by(movieId) %>% summarise(mean = mean(rating), sd = sd(rating), n = n()) %>% 
    ggplot(aes(mean)) + geom_histogram(fill = "darkblue") +
    labs(x = "Rating",
       y = "Frequency")


## ----Genres_vs_Ratings, message = FALSE, warning = FALSE, echo = FALSE--------------------------
genre_dist <- edx %>%
  select(genres, rating) %>% 
  group_by(genres) %>%
  summarize(mean = mean(rating), median = median(rating), n = n()) %>% arrange(desc(mean))

genre_dist %>% mutate(Percentage = paste(round(n / sum(n) * 100, 2), "%", sep = "")) %>% head(15) %>% knitr::kable(caption = "Genres_vs_Ratings")


## ----appearences_top_20, message = FALSE, warning = FALSE, echo = FALSE-------------------------
genre_dist[2:nrow(genre_dist), ] %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  head(15) %>% knitr::kable(caption = "Top_15_Genres")


## ----time_effect_ratings, message = FALSE, warning = FALSE, echo = FALSE------------------------
edx %>% filter(movieId %in% c(2571,296,356,593,480,318,110,457,589,260,150,592,1)) %>% ggplot(aes(timestamp, rating, color = title)) + geom_smooth() + theme(legend.position = "bottom")


## ----Feature_VS_Rating_Correlation,message = FALSE, warning = FALSE, echo = FALSE---------------
library('corrr')
x <- data.frame(userId = as.numeric(edx$userId), movieId = as.numeric(edx$movieId), rating = edx$rating, genres = as.numeric(edx$genres))
x <- as.matrix(x)
correlate(x) %>% knitr::kable(caption = "Feature_VS_Rating_Correlation")



## ----create_train_test, message = FALSE, warning = FALSE----------------------------------------
edx <- edx %>% select(userId, movieId, rating)
set.seed(1, sample.kind="Rounding")
#set.seed(1) #for R version 3.5 and below
test_index <- createDataPartition(edx$rating, times = 1, p = .1, list = FALSE)
# Create the index

train <- edx[-test_index, ] # Create Train set
temp <- edx[test_index, ] # Create Test set

# Make sure userId and movieId in test set are also in train set
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test)
train <- rbind(train, removed)

rm(test_index, temp, removed)


## ----dim_sets, warning = FALSE------------------------------------------------------------------
dim(train)
dim(test)


## ----baseline, message = FALSE, warning = FALSE, echo = FALSE-----------------------------------
mu_hat <- mean(train$rating) # Mean accross all movies.
RMSE_baseline <- RMSE(test$rating, mu_hat) # RMSE in test set.
RMSE_baseline


## ----table_baseline, message = FALSE, warning = FALSE, echo = FALSE-----------------------------
rmse_table <- data_frame(Method = "Baseline", RMSE = RMSE_baseline)
rmse_table %>% knitr::kable(caption = "RMSEs")


## ----normal_model, message = FALSE, warning = FALSE, echo = FALSE-------------------------------
mu <- mean(train$rating)
movie_avgs <- train %>%
  group_by(movieId) %>%
  summarize(m_i = mean(rating - mu))
user_avgs <- test %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(u_i = mean(rating - mu - m_i))
predicted_ratings <- test %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + m_i + u_i) %>% 
  mutate(pred = ifelse(pred < 0, 0, ifelse(pred > 5, 5, pred))) %>% .$pred
#The mutate condition is used to counter values less than 0 and greater than 5.
model_RMSE <- RMSE(predicted_ratings, test$rating)
model_RMSE  


## ----table_usr_mov, message = FALSE, warning = FALSE, echo = FALSE------------------------------
rmse_table <- rbind(rmse_table,
                    data_frame(Method = "User & Movie Effect", RMSE = model_RMSE))
rmse_table %>% knitr::kable(caption = "RMSE")


## ----val_data_set, message = FALSE, warning = FALSE---------------------------------------------
validation <- validation %>% select(userId, movieId, rating)
validation$userId <- as.factor(validation$userId)
validation$movieId <- as.factor(validation$movieId)
validation <- validation[complete.cases(validation), ]


## ----val_user_mov, message = FALSE, warning = FALSE---------------------------------------------
mu <- mean(edx$rating)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(m_i = mean(rating - mu))
user_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(u_i = mean(rating - mu - m_i))
predicted_ratings <- validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + m_i + u_i) %>% mutate(pred = ifelse(pred < 0, 0, ifelse(pred > 5, 5, pred))) %>% .$pred

val_RMSE <- RMSE(predicted_ratings, validation$rating, na.rm = T)

## ----table3_val, message = FALSE, warning = FALSE, echo = FALSE---------------------------------
rmse_table <- rbind(rmse_table,
                    data_frame(Method = "User & Movie Effect on validation", RMSE = val_RMSE))
rmse_table %>% knitr::kable(caption = "RMSE")


## ----user_mov_reg, message = FALSE, warning = FALSE, echo = FALSE-------------------------------
lambda_values <- seq(0, 7, .1)

RMSE_function_reg <- sapply(lambda_values, function(l){
  
  mu <- mean(train$rating)
  
  m_i <- train %>%
    group_by(movieId) %>%
    summarize(m_i = sum(rating - mu)/(n()+l))
  
  u_i <- train %>%
    left_join(m_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(u_i = sum(rating - m_i - mu)/(n()+l))
  
  predicted_ratings <- test %>%
    left_join(m_i, by = "movieId") %>% 
    left_join(u_i, by = "userId") %>%
    mutate(pred = mu + m_i + u_i) %>% mutate(pred = ifelse(pred < 0, 0, ifelse(pred > 5, 5, pred))) %>% .$pred
  
  return(RMSE(predicted_ratings, test$rating))
})


## ----RMSE_curve_test, message = FALSE, warning = FALSE, echo = FALSE----------------------------
qplot(lambda_values, RMSE_function_reg,
      main = "Regularisation",
      xlab = "RMSE", ylab = "Lambda") # lambda vs RMSE


## ----optimum_lambda_test, message = FALSE, warning = FALSE, echo = FALSE------------------------
lambda_opt <- lambda_values[which.min(RMSE_function_reg)]
res_rmse <- min(RMSE_function_reg)
# Lambda which minimizes RMSE


## ----table4, message = FALSE, warning = FALSE, echo = FALSE-------------------------------------
rmse_table <- rbind(rmse_table,
                    data_frame(Method = "User & Movie Effect Regularisation",
                               RMSE = min(RMSE_function_reg)))
rmse_table %>% knitr::kable(caption = "RMSEs")


## ----reg_val, message = FALSE, warning = FALSE--------------------------------------------------
RMSE_function_val_reg <- sapply(lambda_values, function(l){
  
  mu <- mean(edx$rating)
  
  m_i <- edx %>%
    group_by(movieId) %>%
    summarize(m_i = sum(rating - mu)/(n()+l))
  
  u_i <- edx %>%
    left_join(m_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(u_i = sum(rating - m_i - mu)/(n()+l))
  
  predicted_val_reg <- validation %>%
    left_join(m_i, by = "movieId") %>% 
    left_join(u_i, by = "userId") %>%
    mutate(pred = mu + m_i + u_i) %>% mutate(pred = ifelse(pred < 0, 0, ifelse(pred > 5, 5, pred))) %>% .$pred
  
  return(RMSE(predicted_val_reg, validation$rating, na.rm = T))
})


## ----RMSE_Curve_Val, message = FALSE, warning = FALSE, echo = FALSE-----------------------------
qplot(lambda_values, RMSE_function_val_reg,
      main = "Regularisation on validation data set",
      xlab = "Lambda", ylab = "RMSE")


## ----Optimum_Lambda_Val, message = FALSE, warning = FALSE, echo = FALSE-------------------------
lambda_opt_reg <- lambda_values[which.min(RMSE_function_val_reg)]
# Lambda which minimizes RMSE


## ----RMSE_function_val_reg,  message = FALSE, warning = FALSE, echo = FALSE---------------------
min_rmse <- min(RMSE_function_val_reg) # Best RMSE


## ----table_final, message = FALSE, warning = FALSE, echo = FALSE--------------------------------
rmse_table <- rbind(rmse_table,
                    data_frame(Method = "User & Movie Effect Reg. on validation",
                               RMSE = min(RMSE_function_val_reg)))
rmse_table %>% knitr::kable(caption = "RMSEs")

