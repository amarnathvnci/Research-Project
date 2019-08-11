################################################# First part of the project ######################## 
#Loading the packages ----------------------------------------------------
#Pacman library is a combination of various available packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "tidyverse","dplyr", "corrplot","ggcorrplot","Boruta","caret","e1071","randomForest","ROSE","class", "mlbench", "MlBayesOpt", "leaps", "C50")

# Loading The Dataset -----------------------------------------------------
credit_rating <- read.csv("C:/Users/admin/Desktop/Research Project/credit.csv")
credit_df<-credit_rating
View(credit_df)

# Understanding Data ------------------------------------------------------
head(credit_df)  # headers of the file
str(credit_df)  # Structure of Data
sapply(credit_df, FUN=function(x) {sum(is.na(x))}) # Missing Values Check

##########Formating Ratings to numeric form ###########
credit_df$RATING <- as.factor(sapply(credit_rf$RATING, FUN = function(x) {if(x == "AAA" | x == "AA+" | x == "AA" | x == "AA-" | 
                                                                             x == "A+" | x == "A" | x == "A-") {x <- 3} 
  else if (x == "BBB+" | x == "BBB" | x == "BBB-" | x == "BB+" | x == "BB" | x == "BB-" | 
           x == "B+" | x == "B" | x == "B-") {x <- 2} else if (x == "CCC+" | x == "CCC" | x == "CC+" | x == "CC" | x == "CC-" | 
                                                               x == "C+" | x == "C" | x == "C-") {x <- 1}}))
####################################
#Removing missing values as they are structural missing values
credit_df <- na.omit(credit_df)
#######################################


###############################

##########Formatting Ratings to numeric form ###########
credit_df$RATING <- as.factor(sapply(credit_df$RATING, FUN = function(x) {if(x == "AAA" | x == "AA+" | x == "AA" | x == "AA-" | 
                                                                             x == "A+" | x == "A" | x == "A-") {x <- 3} 
  else if (x == "BBB+" | x == "BBB" | x == "BBB-" | x == "BB+" | x == "BB" | x == "BB-" | 
           x == "B+" | x == "B" | x == "B-") {x <- 2} else if (x == "CCC+" | x == "CCC" | x == "CC+" | x == "CC" | x == "CC-" | 
                                                               x == "C+" | x == "C" | x == "C-") {x <- 1}}))
####################################


#######Removing variables other than financial information
credit_df <- credit_df[,4:127]
credit_df$RATING <- droplevels(credit_df$RATING)


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

rfname <- c("COVRAGE","TCE_RATIO","RETURN_ON_ASSET","NONOP_INCOME_LOSS","IS_NET_INTEREST_EXPENSE","SALES_ON_TOT_ASSET",
            "RETURN_ON_CAP","RETURN_ON_INV_CAPITAL","BS_OTHER_ASSETS_DEF_CHRG_OTHER","IS_INT_EXPENSE","BS_MKT_SEC_OTHER_ST_INVEST",
            "DEBT_RATIO","NET_DEBT_TO_SHRHLDR_EQTY","BS_LT_BORROW","NON_CUR_LIAB","BS_TOT_NON_CUR_ASSET","GROSS_MARGIN","CF_INCR_CAP_STOCK",
            "IS_INC_TAX_EXP","NET_DEBT", "RATING")

rfname1 <- c("COVRAGE","TCE_RATIO","RETURN_ON_ASSET","NONOP_INCOME_LOSS","IS_NET_INTEREST_EXPENSE","SALES_ON_TOT_ASSET",
            "RETURN_ON_CAP","RETURN_ON_INV_CAPITAL","BS_OTHER_ASSETS_DEF_CHRG_OTHER","IS_INT_EXPENSE","BS_MKT_SEC_OTHER_ST_INVEST",
            "DEBT_RATIO","NET_DEBT_TO_SHRHLDR_EQTY","BS_LT_BORROW","NON_CUR_LIAB","BS_TOT_NON_CUR_ASSET","GROSS_MARGIN","CF_INCR_CAP_STOCK",
            "IS_INC_TAX_EXP","NET_DEBT")


#########Subsetting dataset with only selected features ############
credit_rf <- credit_df[,rfname]     ####Subsetting dataset with 125 observations
credit_rf$RATING <- droplevels(credit_rf$RATING)

####################################

# Model Building ----------------------------------------------------------
set.seed(123)
my_df1 <- credit_rf
#my_df1$RATING <- as.factor(my_df1$RATING)
#splitIndex1 <- createDataPartition(my_df1$RATING, p=.10, list=FALSE, times=1)
#splitIndex2 <- createDataPartition(my_df1$RATING, p=.20, list=FALSE, times=1)
#my_df2 <- as.data.frame(my_df1[splitIndex2, ]) 
#train <- my_df1[splitIndex1, ]
#test <- my_df2[-splitIndex1, ]

index1 <- createDataPartition(my_df1$RATING, p=.60, list=FALSE, times=1)
train <- my_df1[index1, ]
test <- my_df1[-index1, ]

#test <- my_df1[index1, ]
#train <- my_df1[-index1, ]

#C5.0##########
ip <- train[rfname1]
rp <- train$RATING
C5_mod <- C5.0(x = ip, y = rp)
C5_mod
c5.predict <- predict(C5_mod, test)
confusionMatrix(test$RATING, c5.predict)

#SVM model---
svm.model <- svm(RATING ~ ., data = train, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, test)
confusionMatrix(test$RATING, svm.predict)

# Random Forest -----------------------------------------------------
set.seed(10)
rf.model <- randomForest(RATING ~ ., data = train,
                         ntree = 2000, nodesize = 20)

rf.predict <- predict(rf.model, test)
confusionMatrix(test$RATING, rf.predict)
varImpPlot(rf.model)



##Bayesian Optimisation for SVM 
set.seed(13)
res0 <- svm_cv_opt(data = my_df1,
                   label = RATING,
                   svm_kernel = "polynomial",
                   degree_range = c(2L, 4L),
                   n_folds = 5,
                   kappa = 5,
                   init_points = 4,
                   n_iter = 5)



#Bayesian optimisation for RF - 
set.seed(13)
res1 <- rf_opt(train_data = train,
               train_label = RATING,
               test_data = test,
               test_label = RATING,
               mtry_range = c(1L, ncol(train)-1),
               num_tree = 10L,
               init_points = 4,
               n_iter = 5)

###C50 - Tuning paprameters
set.seed(13)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10, returnResamp="all")
x <- credit_rf[rfname1]
y <- credit_rf$RATING

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )

crcl<- train(x=x,y=y,tuneGrid=grid,trControl=fitControl,method="C5.0",verbose=FALSE)

crcl

# visualize the resample distributions
xyplot(mdl,type = c("g", "p", "smooth"))








#####Model building with 766 observations

credit_rf <- credit_rating[,rfname]  ####Subsetting dataset with 766 observations
credit_rf <- na.omit(credit_rf)
credit_rf$RATING <- droplevels(credit_rf$RATING)
##########Formating Ratings to numeric form ###########Comment the following code to have 14 different classes in Y
credit_rf$RATING <- as.factor(sapply(credit_rf$RATING, FUN = function(x) {if(x == "AAA" | x == "AA+" | x == "AA" | x == "AA-" | 
                                                                             x == "A+" | x == "A" | x == "A-") {x <- 3} 
  else if (x == "BBB+" | x == "BBB" | x == "BBB-" | x == "BB+" | x == "BB" | x == "BB-" | 
           x == "B+" | x == "B" | x == "B-") {x <- 2} else if (x == "CCC+" | x == "CCC" | x == "CC+" | x == "CC" | x == "CC-" | 
                                                               x == "C+" | x == "C" | x == "C-") {x <- 1}}))
####################################


# Model Building ----------------------------------------------------------
set.seed(123)
my_df1 <- credit_rf
#my_df1$RATING <- as.factor(my_df1$RATING)
#splitIndex1 <- createDataPartition(my_df1$RATING, p=.10, list=FALSE, times=1)
#splitIndex2 <- createDataPartition(my_df1$RATING, p=.20, list=FALSE, times=1)
#my_df2 <- as.data.frame(my_df1[splitIndex2, ]) 
#train <- my_df1[splitIndex1, ]
#test <- my_df2[-splitIndex1, ]

index1 <- createDataPartition(my_df1$RATING, p=.60, list=FALSE, times=1)
train <- my_df1[index1, ]
test <- my_df1[-index1, ]

#test <- my_df1[index1, ]
#train <- my_df1[-index1, ]

#C5.0##########
ip <- train[rfname1]
rp <- train$RATING
C5_mod <- C5.0(x = ip, y = rp)
C5_mod
c5.predict <- predict(C5_mod, test)
confusionMatrix(test$RATING, c5.predict)

#SVM model---
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




##Bayesian Optimisation for SVM - division with 766 observations
set.seed(13)
res0 <- svm_cv_opt(data = my_df1,
                   label = RATING,
                   svm_kernel = "polynomial",
                   degree_range = c(1L, 3L),
                   n_folds = 5,
                   kappa = 5,
                   init_points = 4,
                   n_iter = 5)

#Bayesian optimisation for RF - 
set.seed(13)
res1 <- rf_opt(train_data = train,
               train_label = RATING,
               test_data = test,
               test_label = RATING,
               mtry_range = c(1L, ncol(train)-1),
               num_tree = 10L,
               init_points = 4,
               n_iter = 5)

###C50 - Tuning paprameters
set.seed(13)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10, returnResamp="all")
x <- credit_rf[rfname1]
y <- credit_rf$RATING

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )

crcl<- train(x=x,y=y,tuneGrid=grid,trControl=fitControl,method="C5.0",verbose=FALSE)

crcl

# visualize the resample distributions
xyplot(mdl,type = c("g", "p", "smooth"))

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
library(RColorBrewer)
library(sentimentr)

# Reading File -----------------------------------------------------------
apple <- read.csv("C:/Users/admin/Desktop/Research Project/apple.csv") %>% clean_names()

#Numerical consideration of S&P ratings
ratingsp <- data_frame("AAA" = 3, "AA+" = 2.95, "AA" = 2.75, "AA-" = 2.5, "A+" = 2.4, "A" = 2.2, "A-", 2.1, "BBB+" = 2,
                       "BBB" = 1.9, "BBB-" = 1.75, "BB+" = 1.5, "BB" = 1.4, "BB-" = 1.2, "B+" = 1, "B" = 0.9, "B-" = 0.75)

# Exploring Data ----------------------------------------------------------

glimpse(apple)
str(apple)

# Checking number of na
map_df(apple, ~ sum(is.na(.x)))

# All missing value are meaningfully missing values so omitting that
apple <- apple %>% na.omit()

Exploration of dataset
# Checking the word count from tweets about apple
apple$wordcount <- sapply(gregexpr("\\S+", apple$text), length)

ggplot(data = apple, aes(x = wordcount, colour = I("black"), fill = I("#099DD9"))) +
  geom_histogram(binwidth = 3) +
  labs(x = "Word Count", y = "Frequency", title = "Distribution of word count of text")

# Exploration of Most Common Words

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
  labs(title = "Top 15 Significant Words From All tweets", x = "Total Number of Occurances", y = "Word") +
  coord_flip()+  scale_fill_manual(values = getPalette(colourCount))+
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#--------------------------


#Words from Good tweets
grade_words %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(grade) %>% 
  top_n(10) %>% 
  ungroup %>%
  dplyr::filter(grade == "Good")%>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE, fill = 'green') +
  labs(title ="Top 10 Significant Words from Good tweets", x = NULL, y = "tf-idf") +
  coord_flip()+theme_bw()

#Words from Bad tweets
grade_words %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(grade) %>% 
  top_n(10) %>% 
  ungroup %>%
  dplyr::filter(grade == "Bad")%>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE, fill = 'red') +
  labs(title ="Top 10 Significant Words from Bad tweets", x = NULL, y = "tf-idf") +
  coord_flip()

#sentiment analysis using sentiment() function
score <- sentiment(apple$text)
score1 <- aggregate(sentiment~element_id, data=score, FUN=sum) 
apple$score <- score1$sentiment
sum(apple$score)
final_score <- (sum(apple$score) / nrow(apple)) + ratingsp$`AA+`
final_score
