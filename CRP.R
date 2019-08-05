# Loading the packages ----------------------------------------------------
if (!require("pacman")) install.packages("pacman")
#Pacman library is a combination of various available packages 
pacman::p_load("ggplot2", "tidyverse","dplyr", "corrplot","ggcorrplot","Boruta","caret","e1071","randomForest","ROSE","class", "mlbench", "MlBayesOpt", "leaps")

# Loading The Dataset -----------------------------------------------------
credit_rating <- read.csv("C:/Users/admin/Desktop/Research Project/credit.csv")
credit_df<-credit_rating
View(credit_df)

# Understanding Data ------------------------------------------------------
head(credit_df)  # headers of the file
str(credit_df)  # Structure of Data
sapply(credit_df, FUN=function(x) {sum(is.na(x))}) # Missing Values Check

#Removing missing values as they are structural missing values
credit_df <- na.omit(credit_df1)
###############################

##########Formating Ratings to numeric form ###########
credit_df$RATING <- as.factor(sapply(credit_df$RATING, FUN = function(x) {if(x == "AAA" | x == "AA+" | x == "AA" | x == "AA-" | 
                                                                   x == "A+" | x == "A" | x == "A-") {x <- 3} 
                                                               else if (x == "BBB+" | x == "BBB" | x == "BBB-" | x == "BB+" | x == "BB" | x == "BB-" | 
 x == "B+" | x == "B" | x == "B-") {x <- 2} else if (x == "CCC+" | x == "CCC" | x == "CC+" | x == "CC" | x == "CC-" | 
                                                        x == "C+" | x == "C" | x == "C-") {x <- 1}}))
####################################

> #######Removing variables other than financial information
> credit_df <- credit_df[,4:127]
> credit_df$RATING <- droplevels(credit_df$RATING)


#########Variable selection based on importance rate obtained after applying LVQ model
set.seed(7)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(RATING~., data=credit_df, method="lvq", preProcess="scale", trControl=control,tuneGrid = data.frame(size = 124, k = 1:123))
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
########################################

#########Subsetting dataset with only selected features ############
credit_rf <- credit_df[,rfname]
#########################################################

# Model Building ----------------------------------------------------------
set.seed(123)
my_df1 <- credit_rf
#my_df1$RATING <- as.factor(my_df1$RATING)
splitIndex1 <- createDataPartition(my_df1$RATING, p=.10, list=FALSE, times=1)
splitIndex2 <- createDataPartition(my_df1$RATING, p=.20, list=FALSE, times=1)
my_df2 <- as.data.frame(my_df1[splitIndex2, ]) 
train <- my_df1[splitIndex1, ]
test <- my_df2[-splitIndex1, ]

# KNN ---------------------------------------------------------------------

knn1 <- knn(train = train, test = test, cl = train$RATING, k = 3)
confusionMatrix(knn1, test$RATING)
#######################################

#SVM model
svm.model <- svm(RATING ~ ., data = train, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, test)
confusionMatrix(test$RATING, svm.predict)

# Random Forest -----------------------------------------------------------
set.seed(10)
rf.model <- randomForest(RATING ~ ., data = train,
                         ntree = 2000, nodesize = 20)

rf.predict <- predict(rf.model, test)
confusionMatrix(test$RATING, rf.predict)
varImpPlot(rf.model)

#Random forest tuning using caret (source: https://rpubs.com/phamdinhkhanh/389752)

x <- my_df1[,1:20]
y <- my_df1[,21]

#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)
#Number randomely variable selected is mtry
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(RATING~., 
                    data=my_df1, 
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_default)


#Random search

# library(doParallel)
# cores <- 7
# registerDoParallel(cores = cores)
#mtry: Number of random variables collected at each split. In normal equal square number columns.
mtry <- sqrt(ncol(x))
#ntree: Number of trees to grow.
ntree <- 3


control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')

#Random generate 15 mtry values with tuneLength = 15
set.seed(1)
rf_random <- train(RATING ~ .,
                   data = my_df1,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
print(rf_random)


##Bayesian Optimisation for SVM - Obtained highest accuracy of over 91%
res0 <- svm_cv_opt(data = my_df1,
                     label = RATING,
                     svm_kernel = "polynomial",
                     degree_range = c(2L, 4L),
                     n_folds = 3,
                     kappa = 5,
                     init_points = 4,
                     n_iter = 5)

#Bayesian optimisation for RF - obtained highest accuracy of over 95%
res1 <- rf_opt(train_data = train,
               train_label = RATING,
               test_data = test,
               test_label = RATING,
               mtry_range = c(1L, ncol(train)-1),
               num_tree = 10L,
               init_points = 4,
               n_iter = 5)

###############################  PART 02  ####################################### 
#Continuous credit rating via Sentiment Analysis
# Installing packages
install.packages("tidyverse")
install.packages("janitor")
install.packages("purr")
install.packages("psych")
install.packages("tidytext")
install.packages("randomcoloR")
install.packages("broom")
install.packages("ggrepel")
install.packages("caret")
install.packages("e1071")
install.packages("randomForest")
install.packages("RColorBrewer")
install.packages("sentimentr")
# Loading Libraries -------------------------------------------------------
library(tidyverse)
library(janitor)
library(purrr)
library(psych)
library(tidytext)
library(randomcoloR)
library(tidytext)
library(broom)
library(ggrepel)
library(caret)
library(e1071)
library(randomForest)
library(RColorBrewer)
library(sentimentr)

# Reading File -----------------------------------------------------------
apple <- read.csv("C:/Users/admin/Desktop/Research Project/apple.csv") %>% clean_names()


# Exploring Data ----------------------------------------------------------

glimpse(apple)
str(apple)

# Checking number of na
map_df(apple, ~ sum(is.na(.x)))

# All missing value are meaningfully missing values so omitting that
apple <- apple %>% na.omit()

# Checking distribution + correlation using pairs.panels
pairs.panels(apple[1:100, 2:5])
pairs.panels(apple[1:100, 6:11])


# Checking the word count from tweets about apple
apple$wordcount <- sapply(gregexpr("\\S+", apple$text), length)

ggplot(data = apple, aes(x = wordcount, colour = I("black"), fill = I("#099DD9"))) +
  geom_histogram(binwidth = 3) +
  labs(x = "Word Count", y = "Frequency", title = "Distribution of word count of text")

# Explotation of Most Common Words

apple$text <- as.character(apple$text)
apple$sentiment <- as.numeric(apple$sentiment)

apple = apple %>%
  mutate(grade = ifelse(sentiment > "3","Good",ifelse(sentiment > "2","Average","Bad")))
#Creating Grade column based on points

apple$grade = as.factor(apple$grade)
apple$grade = factor(apple$grade,levels(apple$grade)[c(3,1,2)])

#Finding most common words
apple_Words = apple %>% dplyr::select(text, grade)%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  dplyr::filter(!str_detect(word, "[0-9]"))

colourCount = 15
getPalette = colorRampPalette(brewer.pal(9, "Set1"))


apple_Words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_col() +
  labs(title = "Top 15 Significant Words From All Comments", x = "Total Number of Occurances", y = "Word") +
  coord_flip()+  scale_fill_manual(values = getPalette(colourCount))+
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#--------------------------

comment_words = apple_Words %>%
  count(grade, word, sort = TRUE) %>%
  ungroup()

total_words = comment_words %>% 
  group_by(grade) %>% 
  summarize(total = sum(n))

grade_words = left_join(comment_words, total_words)

grade_words = grade_words %>%
  bind_tf_idf(word, grade, n)


#Words from Good review
grade_words %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(grade) %>% 
  top_n(10) %>% 
  ungroup %>%
  dplyr::filter(grade == "Good")%>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE, fill = 'red') +
  labs(title ="Top 10 Significant Words from Good posts", x = NULL, y = "tf-idf") +
  coord_flip()+theme_bw()

#Words from Bad review
grade_words %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(grade) %>% 
  top_n(10) %>% 
  ungroup %>%
  dplyr::filter(grade == "Bad")%>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE, fill = 'Blue') +
  labs(title ="Top 10 Significant Words from Bad reviews", x = NULL, y = "tf-idf") +
  coord_flip()

#sentiment analysis using sentiment() function
score <- sentiment(apple$text)
score1 <- aggregate(sentiment~element_id, data=score, FUN=sum) 
apple$score <- score1$sentiment

#Sentiment Analysis

word_final <- inner_join(apple_Words, grade_words[,c("grade","word","tf")], by=c("grade","word"))

word_final <- inner_join(word_final, get_sentiments("afinn"))

sentiment_score <- word_final %>% group_by(grade) %>% summarise(sentiment_score=mean(value)) 

apple_final <- full_join(apple, sentiment_score)

apple_final %>% group_by(grade) %>% summarise(mean(sentiment_score,na.rm = TRUE))

temp <- apple_final %>% dplyr::filter(is.na(sentiment_score)) %>% 
  mutate(sentiment_score= ifelse(grade=="Good",1.76,ifelse(grade=="Average",1.54,1.13)))

apple_final <- apple_final %>% na.omit()

apple <- unique(rbind(apple_final,temp))

head(apple)

