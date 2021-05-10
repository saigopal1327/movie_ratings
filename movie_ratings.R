##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(amap)) install.packages("amap", repos = "http://cran.us.r-project.org") # package required

library(tidyverse)
library(caret)
library(data.table)
library(lubridate) # used for wrangling timestamp data
library(amap) # this package is for calcualting correlation distance

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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


############################
# Description of the Dataset
############################

dim(edx)         # number of rows and column in the data set

head(edx, n = 3) # print the specified (n) of rows of the dataset

any(is.na(edx))  # is.na returns TRUE if there are missing values
                 # any function returns TRUE if any element inside is TRUE
                 # FALSE if there are no TRUE
                 # there no missing values in edx data

##############################################
# Exploratory Data Analysis and Data Wrangling 
##############################################

#############################################################
# Number and distribution of user's and movie's in edx dataset
#############################################################

cat("Number of User's =", n_distinct(edx$userId))    # cat function prints 
                                                     # the object supplied
                                                     # n_distinct returns the 
                                                     # sum of unique elements
                                                     # in a vector (userId)

cat("Number of Movie's = ", n_distinct(edx$movieId)) # same as above for movies

edx_user_dat<- edx %>% group_by(userId) %>%   # creating a dataframe with userId
                                              # number of times they rated and Group
  summarise(number_rated = n()) %>%           # indicating whether the user's are in
                                              # top five or bottom five percentile
  mutate(Group = case_when(
    number_rated < quantile(number_rated, probs = 0.05) ~ "Bottom five",
    number_rated > quantile(number_rated, probs = 0.95) ~ "Top five"
  ))
    
edx_user_dat<- edx_user_dat %>%
  mutate(Group = ifelse(is.na(Group), "Middle ninety", Group)) # assigning the 
                                                               # rest of user's as 
                                                               # as "Middle ninety"


edx_movie_dat<- edx %>% group_by(movieId) %>%                  # same above for
                                                               # but for movie data
  summarise(number_rated = n()) %>%           
  mutate(Group = case_when(
    number_rated < quantile(number_rated, probs = 0.05) ~ "Bottom five",
    number_rated > quantile(number_rated, probs = 0.95) ~ "Top five"
  ))

edx_movie_dat<- edx_movie_dat %>%                             # same as done for user's
  mutate(Group = ifelse(is.na(Group), "Middle ninety", Group)) 

cat("User's Distribution in dataset") # for identifying user's distribution table below

edx_user_dat %>% group_by(Group) %>%  # creating table to display the number of users
                                      # number of times they rated and the proportion of 
                                      # their rating wrt edx dataset
  summarise(number = n(), total_ratings = sum(number_rated)) %>%
  mutate(percentage_dataset = total_ratings/nrow(edx)) %>% kable()

cat("Movies's Distribution in dataset") # for identifying movie's distribution table below

edx_movie_dat %>% group_by(Group) %>% # same as done for user's
  summarise(number = n(), total_ratings = sum(number_rated)) %>%
  mutate(percentage_dataset = total_ratings/nrow(edx)) %>% kable()

##########################################
# Creating new variables from timestamp
##########################################

library(lubridate)

edx<- edx %>% mutate(rating_year = year(as_datetime(timestamp)), # as_time function to convert timestamp into 
                                                                 # date and year function extract year from date
                     day_week = weekdays(as_datetime(timestamp))) # weekday function to convert date into 
                                                                  # day of the week 

########################################
# Distribution of ratings by rating year
########################################

edx %>% ggplot(aes(x = factor(rating_year), y = rating)) +
  geom_boxplot() + ylab("Ratings") + xlab("Rating Year") +
  ggtitle("Distribution of Rating by Rating Year") + 
  stat_summary(fun = mean, geom="point", size = 2) + # stat_summary to add mean to the plot
  theme_minimal()

sum(edx$rating_year == 1995) # only two observation for the year 1995

###############################################################
# table- number, average rating and sd of rating by day of week
###############################################################

edx$day_week<- factor(edx$day_week, levels = c("Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday", "Saturday",
                                               "Sunday")) # for making the day in a meaningful
                                                          # order for tabulating data

edx %>% group_by(day_week) %>%
  summarise(Number = n(), Mean = mean(rating), SD = sd(rating)) %>% data.table() # mean rating by day of week table

#####################################################################
# Creating variable for the year the movie released from title column
#####################################################################

film_year_pattern<- "\\(19[0-9][0-9]\\)|\\(20[0-9][0-9]\\)" # regex pattern for four number characters
                                                            # within parenthesis; the first two
                                                            # characters should be 19 or 20
                                                            # followed by an 2 character number

edx<- edx %>% mutate(film_year = str_extract(title, pattern = film_year_pattern)) %>% 
  mutate(film_year = str_remove(film_year, pattern = "\\(")) %>%        # removing parenthesis
  mutate(film_year = as.numeric(str_remove(film_year, pattern = "\\)"))) # removing parenthesis 
                                                                        # and converting character into numeric

######################################################################
# Visualization of average rating of movies vs year the movie released
######################################################################

edx %>% group_by(film_year) %>% 
  summarise(avg_rating = mean(rating), number_movies = n()) %>% 
  ggplot(aes(x = film_year, y = avg_rating)) + 
  geom_point(aes(size = sqrt(number_movies)), alpha = 0.2) +          # alpha for seeing overlaid points
  ylab("Average Rating") + xlab("Year Film Released") +               # x and y labels
  ggtitle("Avearge Rating Vs Year Movie Released") +                  # title of figure
  scale_x_continuous(breaks = seq(from = 1910, to = 2010, by = 10)) + # x axis break 
  geom_vline(xintercept = 1980, col = "brown", lty = "dashed") +      # adding vertical line for year 1980
  scale_size_continuous(name = "Number of Movies (Sqrt)") +           # changing name in legend
  geom_smooth(method = "lm", col = "orange") + theme_minimal() 

edx<- edx %>% mutate(film_1980 = ifelse(film_year > 1980, 1, 0)) # creating binary variable for movies
                                                                 # released after 1980 = 1 and 0 if otherwise

average_before_1980<- mean(edx$rating[edx$film_1980 == 0]) # returns the mean rating for movies before 1980
average_after_1980<- mean(edx$rating[edx$film_1980 == 1])  # returns the mean rating for movies after 1980

cat("Mean rating: Movies before 1980 = ", average_before_1980) # printing the mean with group name 
cat("Mean rating: Movies after 1980 = ", average_after_1980)


############################################################################
# Hierarchical Clustering analysis to identify movies with similar residuals
############################################################################

######################################################################
# Extracting the movieId of movies rated above the 95 th percentile
######################################################################

movies_index<- edx %>% group_by(movieId) %>%
  summarise(number_rated = n()) %>%
  filter(number_rated > quantile(number_rated, probs = 0.95)) %>% # quantile function to filter the top five
                                                                  # movieId
  pull(movieId)

length(movies_index) # number of movies in movies in top five

#########################################################################
# Filtering movies in movies_index and changing users as column variables
#########################################################################

cluster_dat<-edx %>% filter(movieId %in% movies_index) %>%
  group_by(userId) %>%
  filter(n() > 50) %>% ungroup() %>%     # distance calculation are sensitive to missing values, this 
                                         # somewhat reduces missing value
  select(title, userId, rating) %>%
  spread(key = userId, value = rating)   # spread function converts userId to a column variable

###############################################################################
# Saving the title in a separate vector and converting cluster_dat into matrix
###############################################################################

row_names <- cluster_dat$title # saving row names

cluster_matrix <- cluster_dat[,- 1] %>% as.matrix() # removing title - converting into matrix

cluster_matrix <- sweep(cluster_matrix, 2, colMeans(cluster_matrix, na.rm = TRUE)) # standardization of ratings

cluster_matrix <- sweep(cluster_matrix, 1, rowMeans(cluster_matrix, na.rm = TRUE)) # same

rownames(cluster_matrix) <- row_names # adding names to matrix for later indexing

####################################################
# Calculating correlation distance among the movies
####################################################

library(amap)

d<- Dist(cluster_matrix, method = "correlation") # function use pairwise complete observation by default
                                                # allows NA

###########################################################
# Hierarchical clustering and cutting into clusters (group)
###########################################################

h<- hclust(d, method = "ward.D2") # default method is "complete", which measures distance as distance between 
                                  # to farthest point in a cluster. "ward.D2" minimize within cluster variance

plot(h, cex = 0.05, ann = F)      # annotation (logical- true or false) if F = remove x and y x axis labels 
                                  # and default headers

abline(h = 2.7, lty = "dashed", col = "brown") # to add a line to the plot; 
                                               # h stands for horizontal and value the y intercept,  
                                               # lty = line type

movies_clusters <- cutree(h, h = 2.7)    # cuts the dendrogram created above the given values; h stand for height, 
                                      
table(movies_clusters)

g1<- names(movies_clusters)[movies_clusters==1] # identifying the movie in each group for creating new variables
g2<- names(movies_clusters)[movies_clusters==2]
g3<- names(movies_clusters)[movies_clusters==3]
g4<- names(movies_clusters)[movies_clusters==4]
g5<- names(movies_clusters)[movies_clusters==5]
g6<- names(movies_clusters)[movies_clusters==6]
g7<- names(movies_clusters)[movies_clusters==7]
g8<- names(movies_clusters)[movies_clusters==8]

##################################
# Examples of movies in the groups
##################################

g3[str_detect(g3, "Donnie Br|Godfa|Goodfe|Carlito|Heat|Casino")] # Gangster and Al Pacino movies

g5[c(22,23,29,30,36,37,10,11,5,6,12,13)] # nerd movies

#############################################
# Creating a variable to capture these groups
#############################################

edx<- edx %>% mutate(groups = case_when(      # for coding new column, for movie groups (1,2...8)
  title %in% g1 ~ "Group 1",                  # %in% means in and it returns a logical (yes/no)
  title %in% g2 ~ "Group 2",
  title %in% g3 ~ "Group 3",
  title %in% g4 ~ "Group 4",
  title %in% g5 ~ "Group 5",
  title %in% g6 ~ "Group 6",
  title %in% g7 ~ "Group 7",
  title %in% g8 ~ "Group 8"
))

edx<- edx %>% mutate(groups = factor(ifelse(is.na(groups), "Group 0", groups))) # assigning group 0 
                                                                                # to movies not in 8 group

#######################################################
# Visualization of distribution of rating in the groups
#######################################################

edx %>% ggplot(aes(x = groups, y = rating)) + 
  geom_boxplot() + xlab("Movie Cluster") +
  ylab("Rating") + ggtitle("Distribution of Rating by Groups") +
  stat_summary(fun = mean, geom="point", shape = 1, size = 2) +   # stat_summary to add function=mean to plot
  theme_minimal()

############################################
# Creating binary variables for each cluster
############################################

edx<- edx %>%
  mutate(group1 = ifelse(title %in% g1, 1, 0),
         group2 = ifelse(title %in% g2, 1, 0),
         group3 = ifelse(title %in% g3, 1, 0),
         group4 = ifelse(title %in% g4, 1, 0),
         group5 = ifelse(title %in% g5, 1, 0),
         group6 = ifelse(title %in% g6, 1, 0),
         group7 = ifelse(title %in% g7, 1, 0),
         group8 = ifelse(title %in% g8, 1, 0)) # creating binary variable to each of the
                                               # eight group. Group 0 will be 0 in all eight groups
                                               # [lm function does this internally]


###############################
# Identifying all unique genres
###############################

unique(unlist(str_split(edx$genres, pattern = "\\|"))) #\\ escape character for pipe. 
                                                       # | has special meaning [or] in regex

#############################################################################
# Creating column variables for all unique genres except "(no genres listed)"
#############################################################################

edx<- edx %>% mutate(comedy = ifelse(str_detect(genres, pattern = "Comedy"), 1, 0),  # str_detect returns logical and if yes 1 is assigned, 0 otherwise
                     romance = ifelse(str_detect(genres, pattern = "Romance"), 1, 0), 
                     action = ifelse(str_detect(genres, pattern = "Action"), 1, 0), 
                     crime = ifelse(str_detect(genres, pattern = "Crime"), 1, 0),
                     thriller = ifelse(str_detect(genres, pattern = "Thriller"), 1, 0),
                     drama = ifelse(str_detect(genres, pattern = "Drama"), 1, 0),
                     sci_fi = ifelse(str_detect(genres, pattern = "Sci-Fi"), 1, 0),
                     adventure = ifelse(str_detect(genres, pattern = "Adventure"), 1, 0),
                     children = ifelse(str_detect(genres, pattern = "Children"), 1, 0),
                     fantasy = ifelse(str_detect(genres, pattern = "Fantasy"), 1, 0),
                     war = ifelse(str_detect(genres, pattern = "War"), 1, 0),
                     animation = ifelse(str_detect(genres, pattern = "Animation"), 1, 0),
                     musical = ifelse(str_detect(genres, pattern = "Musical"), 1, 0),
                     western = ifelse(str_detect(genres, pattern = "Western"), 1, 0),
                     mystery = ifelse(str_detect(genres, pattern = "Mystery"), 1, 0),
                     film_noir = ifelse(str_detect(genres, pattern = "Film-Noir"), 1, 0),
                     horror = ifelse(str_detect(genres, pattern = "Horror"), 1, 0),
                     documentary = ifelse(str_detect(genres, pattern = "Documentary"), 1, 0),
                     imax = ifelse(str_detect(genres, pattern = "IMAX"), 1, 0))

################################
# Table - mean rating by genre
################################

rbind(edx %>% group_by(comedy) %>% summarise(Genre = "Comedy", Mu = mean(rating)) %>%
        spread(key = comedy, value = Mu),                                                    
                                                                                            
      edx %>% group_by(romance) %>% summarise(Genre = "Romance", Mu = mean(rating)) %>%
        spread(key = romance, value = Mu),
      
      edx %>% group_by(action) %>% summarise(Genre = "Action", Mu = mean(rating)) %>%
        spread(key = action, value = Mu),
      
      edx %>% group_by(crime) %>% summarise(Genre = "Crime", Mu = mean(rating)) %>%
        spread(key = crime, value = Mu),
      
      edx %>% group_by(thriller) %>% summarise(Genre = "Thriller", Mu = mean(rating)) %>%
        spread(key = thriller, value = Mu),
      
      edx %>% group_by(drama) %>% summarise(Genre = "Drama", Mu = mean(rating)) %>%
        spread(key = drama, value = Mu),
      
      edx %>% group_by(sci_fi) %>% summarise(Genre = "Sci-Fi", Mu = mean(rating)) %>%
        spread(key = sci_fi, value = Mu),
      
      edx %>% group_by(adventure) %>% summarise(Genre = "Adventure", Mu = mean(rating)) %>%
        spread(key = adventure, value = Mu),
      
      edx %>% group_by(children) %>% summarise(Genre = "Children", Mu = mean(rating)) %>%
        spread(key = children, value = Mu),
      
      edx %>% group_by(fantasy) %>% summarise(Genre = "Fantasy", Mu = mean(rating)) %>%
        spread(key = fantasy, value = Mu),
      
      edx %>% group_by(war) %>% summarise(Genre = "War", Mu = mean(rating)) %>%
        spread(key = war, value = Mu),
      
      edx %>% group_by(animation) %>% summarise(Genre = "Animation", Mu = mean(rating)) %>%
        spread(key = animation, value = Mu),
      
      edx %>% group_by(musical) %>% summarise(Genre = "Musical", Mu = mean(rating)) %>%
        spread(key = musical, value = Mu),
      
      edx %>% group_by(western) %>% summarise(Genre = "Western", Mu = mean(rating)) %>%
        spread(key = western, value = Mu),
      
      edx %>% group_by(mystery) %>% summarise(Genre = "Mystery", Mu = mean(rating)) %>%
        spread(key = mystery, value = Mu),
      
      edx %>% group_by(film_noir) %>% summarise(Genre = "Film-Noir", Mu = mean(rating)) %>%
        spread(key = film_noir, value = Mu),
      
      edx %>% group_by(horror) %>% summarise(Genre = "Horror", Mu = mean(rating)) %>%
        spread(key = horror, value = Mu),
      
      edx %>% group_by(documentary) %>% summarise(Genre = "Documentary", Mu = mean(rating)) %>%
        spread(key = documentary, value = Mu),
      
      edx %>% group_by(imax) %>% summarise(Genre = "IMAX", Mu = mean(rating)) %>%
        spread(key = imax, value = Mu)) %>% rename(No = '0', Yes = '1') %>%
  
  mutate(Difference = Yes - No) %>% arrange(desc(abs(Difference))) # arranging in magnitude size to make 
                                                                   # viewing easier

#############################
# Model building and testing 
#############################

###############################################
# For readers not familiar with matrix algebra
###############################################

dat<- data.frame(x1 = c(rep("Yes", 4), rep("No", 4)), y =rnorm(8))
dat

lm(y ~ x1, data = dat)$coeff

y<- dat$y

X<- dat %>% mutate(x1 = ifelse(x1 == "Yes", 1, 0), intercept = 1) %>% 
  select(x1, intercept) %>% as.matrix()
X

betahats<- solve(crossprod(X)) %*% crossprod(X, y)

betahats
remove(X, y, dat, betahats)

###########################################################
# Creating objects and functions require for model building
###########################################################

#####################################################
# splitting edx dataset into train set and test set
#####################################################

set.seed(27, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(3)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.10, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

###############################################################
# Make sure userId and movieId in test set are also in train set
###############################################################

test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

####################################################
# Add rows removed from test set back into train set
####################################################

removed <- anti_join(temp, test_set)
train_set<- rbind(train_set, removed)
rm(temp, test_index, removed)

#############################################
# RMSE function, true_ratings, train_mu and y
#############################################

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

true_ratings<- test_set$rating

train_mu<- mean(train_set$rating)

y<- train_set$rating       # for matrix solution of LSE

####################################
# Null model (train mu) without bias
####################################

rmse<- RMSE(true_ratings = true_ratings, predicted_ratings = train_mu)

models<- data.frame(Model = "Null model (Naive Mu)", RMSE = rmse)

models


#############################################################################
# LSE Mu -  predictors [column 10, 12:38] and 39 is intercept
#############################################################################

true_ratings<- test_set$rating

y<- train_set$rating

X<- train_set %>% mutate(intercept = 1) %>% 
  select(10, 12:39) %>% as.matrix() # column 10 is film_year, 12 to 38 are
# movie cluster and genres, 39 intercept

test_matrix<- test_set %>% mutate(intercept = 1) %>%
  select(10, 12:39) %>% as.matrix()

betahats<- solve(crossprod(X)) %*% crossprod(X, y)

predicted_ratings<- test_matrix %*% betahats

rmse<- RMSE(true_ratings = true_ratings, predicted_ratings = predicted_ratings)

models<- rbind(models, c("LSE Mu", rmse))

remove(X, test_matrix, rmse, predicted_ratings)

models

#####################################################
# coefficient of LSE model without bias (above model)
#####################################################

coef_names<- names(train_set)[c(10, 12:38)]  # coefficient names from column of train_set
coef_names<- c(coef_names, "intercept")      # adding intercept to the name
dat<- data.frame(ceoffs_name = coef_names, estimate = betahats) # creating dataframe with coeff names and 
                                                                # their estimate
dat %>% filter(estimate > 0.3 | estimate < - 0.3) %>% # filtering only large estimate
  arrange(desc(abs(estimate)))                        # arranging by absolute magnitude
remove(coef_names, dat, betahats)


################################
# Naive mu + movie and user bias
################################

train_movie_avgs <- train_set %>% select(movieId, userId, rating) %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - train_mu))

train_user_avgs <- train_set %>% select(movieId, userId, rating) %>%
  left_join(train_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - train_mu - b_i))

predicted_ratings <- test_set %>% select(movieId, userId, rating) %>%
  left_join(train_movie_avgs, by='movieId') %>%
  left_join(train_user_avgs, by='userId') %>%
  mutate(pred = train_mu + b_i + b_u) %>%
  pull(pred)

rmse<- RMSE(true_ratings = true_ratings, predicted_ratings = predicted_ratings)

models<- rbind(models, c("Naive Mu + Movie bias + User bias", rmse))

remove(train_movie_avgs, train_user_avgs, predicted_ratings, rmse)

models

#############################
# LSE Mu + movie + user bias
############################

train_mu<- mean(train_set$rating)

train_bi <- train_set %>% select(userId, movieId, rating) %>%
  group_by(movieId) %>% summarise(b_i = mean(rating - train_mu))

train_bu<- train_set %>% select(userId, movieId, rating) %>%
  left_join(train_bi, by="movieId") %>%
  group_by(userId) %>% summarise(b_u = mean(rating - train_mu - b_i))


X<- train_set %>% group_by(movieId) %>%
  mutate(b_i = mean(rating - train_mu)) %>% ungroup() %>%
  group_by(userId) %>% mutate(b_u = mean(rating - train_mu - b_i)) %>% 
  ungroup() %>%
  mutate(intercept = 1) %>%
  select(10, 12:41) %>%  # column 10 is film_year, 12 to 38 movie groups
  # and genres, 39, 40 and 41 - b_i, b_u and intercept
  as.matrix()

test_matrix<- test_set %>%
  left_join(train_bi, by = "movieId") %>%
  left_join(train_bu, by = "userId") %>%
  mutate(intercept = 1) %>%
  select(10, 12:41) %>%
  as.matrix()

betahats<- solve(crossprod(X)) %*% crossprod(X, y) 

predicted_ratings<- test_matrix %*% betahats

rmse<- RMSE(true_ratings = true_ratings, predicted_ratings = predicted_ratings)

models<- rbind(models, c("LSE Mu + Movie Bias + User Bias", rmse))

remove(X, test_matrix, predicted_ratings, rmse,train_bi,train_bu)

models

#####################################################
# coefficient of LSE model with user bias 
#####################################################

coef_names<- names(train_set)[c(10, 12:38)]
coef_names<- c(coef_names, "b_i", "b_u", "intercept")  # adding b_i, b_u and intercept to coef_names
dat<- data.frame(ceoffs_name = coef_names, estimate = betahats)
dat %>% filter(estimate > 0.05 | estimate < - 0.03) %>% arrange(desc(abs(estimate)))
remove(coef_names, dat, betahats)

##################################################################################
# Regularization and plotting of RMSE Vs Lamda's grouped by method (Naive and LSE)
###################################################################################

lamdas<- seq(2, 9, 1)

rmses_lse_mu <- sapply(lamdas, function(l){
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - train_mu)/(n()+l))
  
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - train_mu)/(n()+l))
  
  X<- train_set %>% 
    group_by(movieId) %>%
    mutate(b_i = sum(rating - train_mu)/(n()+l)) %>%
    ungroup() %>%
    group_by(userId) %>% 
    mutate(b_u = sum(rating - b_i - train_mu)/(n()+l)) %>% 
    ungroup() %>%
    mutate(intercept = 1)  %>%
    select(10, 12:41) %>%
    as.matrix()
  
  test_matrix<- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(intercept = 1)  %>%
    select(10, 12:41) %>%
    as.matrix() 
  
  betahats<- solve(crossprod(X)) %*% crossprod(X, y) 
  
  predicted_ratings<- test_matrix %*% betahats
  
  return(RMSE(true_ratings = true_ratings, predicted_ratings = predicted_ratings))
})

rmses_naive_mu <- sapply(lamdas, function(l){
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - train_mu)/(n()+l))
  
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - train_mu)/(n()+l))
  
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = train_mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(true_ratings = true_ratings, predicted_ratings = predicted_ratings))
})

rmses_dat<- data.frame(lamdas = lamdas, rmses_lse_mu = rmses_lse_mu,
                       rmses_naive_mu = rmses_naive_mu) # creating dataframe with lamda, rmses with naive 
                                                        # and rmses with lse model

remove(rmses_naive_mu, rmses_lse_mu, lamdas)

rmses_dat %>% gather("Method", "RMSE", - lamdas) %>% 
  mutate(Method = factor(Method, labels = c("LSE Mu", "Naive Mu"))) %>%
  ggplot(aes(x = lamdas, y = RMSE, col = Method)) +    # assigning color to method
  geom_point() + theme_minimal()

remove(rmses_dat)

####################################################
# Obtaining the model betahats from full edx dataset
####################################################

lamda<- 5

edx_mu<- mean(edx$rating)

y<- edx$rating

edx_b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - edx_mu)/(n()+lamda))

edx_b_u <- edx %>%
  left_join(edx_b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - edx_mu)/(n()+lamda))

X<- edx %>% 
  group_by(movieId) %>%
  mutate(b_i = sum(rating - edx_mu)/(n()+lamda)) %>%
  ungroup() %>%
  group_by(userId) %>% 
  mutate(b_u = sum(rating - b_i - edx_mu)/(n()+lamda)) %>% 
  ungroup() %>%
  mutate(intercept = 1)  %>%
  select(10, 12:41) %>%
  as.matrix() 

edx_betahats<- solve(crossprod(X)) %*% crossprod(X, y)

remove(X, y, edx_mu, edx, lamda)

######################################
# Creating variables in validation set (those created for edx data are also created for validation)
#####################################

validation<- validation %>% mutate(rating_year = year(as_datetime(timestamp)), 
                                   day_week = weekdays(as_datetime(timestamp)))  


validation<- validation %>% mutate(film_year = str_extract(title, pattern = film_year_pattern)) %>% 
  mutate(film_year = str_remove(film_year, pattern = "\\(")) %>%        
  mutate(film_year = as.numeric(str_remove(film_year, pattern = "\\)")))

validation<- validation %>% mutate(film_1980 = ifelse(film_year > 1980, 1, 0))

validation<- validation %>% mutate(groups = case_when(
  title %in% g1 ~ "Group 1",
  title %in% g2 ~ "Group 2",
  title %in% g3 ~ "Group 3",
  title %in% g4 ~ "Group 4",
  title %in% g5 ~ "Group 5",
  title %in% g6 ~ "Group 6",
  title %in% g7 ~ "Group 7",
  title %in% g8 ~ "Group 8"
))

validation<- validation %>% mutate(groups = factor(ifelse(is.na(groups), "Group 0", groups)))

validation<- validation %>%
  mutate(group1 = ifelse(title %in% g1, 1, 0),
         group2 = ifelse(title %in% g2, 1, 0),
         group3 = ifelse(title %in% g3, 1, 0),
         group4 = ifelse(title %in% g4, 1, 0),
         group5 = ifelse(title %in% g5, 1, 0),
         group6 = ifelse(title %in% g6, 1, 0),
         group7 = ifelse(title %in% g7, 1, 0),
         group8 = ifelse(title %in% g8, 1, 0))


validation<- validation %>% mutate(comedy = ifelse(str_detect(genres, pattern = "Comedy"), 1, 0), 
                                   romance = ifelse(str_detect(genres, pattern = "Romance"), 1, 0), 
                                   action = ifelse(str_detect(genres, pattern = "Action"), 1, 0), 
                                   crime = ifelse(str_detect(genres, pattern = "Crime"), 1, 0),
                                   thriller = ifelse(str_detect(genres, pattern = "Thriller"), 1, 0),
                                   drama = ifelse(str_detect(genres, pattern = "Drama"), 1, 0),
                                   sci_fi = ifelse(str_detect(genres, pattern = "Sci-Fi"), 1, 0),
                                   adventure = ifelse(str_detect(genres, pattern = "Adventure"), 1, 0),
                                   children = ifelse(str_detect(genres, pattern = "Children"), 1, 0),
                                   fantasy = ifelse(str_detect(genres, pattern = "Fantasy"), 1, 0),
                                   war = ifelse(str_detect(genres, pattern = "War"), 1, 0),
                                   animation = ifelse(str_detect(genres, pattern = "Animation"), 1, 0),
                                   musical = ifelse(str_detect(genres, pattern = "Musical"), 1, 0),
                                   western = ifelse(str_detect(genres, pattern = "Western"), 1, 0),
                                   mystery = ifelse(str_detect(genres, pattern = "Mystery"), 1, 0),
                                   film_noir = ifelse(str_detect(genres, pattern = "Film-Noir"), 1, 0),
                                   horror = ifelse(str_detect(genres, pattern = "Horror"), 1, 0),
                                   documentary = ifelse(str_detect(genres, pattern = "Documentary"), 1, 0),
                                   imax = ifelse(str_detect(genres, pattern = "IMAX"), 1, 0))

######################################
# Applying the model on validation set
######################################

validation_matrix<- validation %>%
  left_join(edx_b_i, by = "movieId") %>%
  left_join(edx_b_u, by = "userId") %>%
  mutate(intercept = 1)  %>%
  select(10, 12:41) %>%
  as.matrix() 

colnames(validation_matrix)

predicted_ratings<- validation_matrix %*% edx_betahats

Final_RMSE<- RMSE(true_ratings = validation$rating, predicted_ratings = predicted_ratings)

Final_RMSE





