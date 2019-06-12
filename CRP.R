# Loading the packages ----------------------------------------------------
#Pacman library is a combination of various available packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "tidyverse","dplyr", "corrplot","ggcorrplot","Boruta","caret","e1071","randomForest","ROSE","class")

# Loading The Dataset -----------------------------------------------------
credit_rating <- read.csv("C:/Users/admin/Desktop/Research Project/credit.csv")
credit_df<-credit_rating
View(credit_df)

# Understanding Data ------------------------------------------------------
head(credit_df)  # headers of the file
str(credit_df)  # Structure of Data
sapply(credit_df, FUN=function(x) {sum(is.na(x))}) # Missing Values Check

##########Formating Ratings to numeric form ###########
credit_df$RATING <- as.factor(sapply(credit_df$RATING, FUN = function(x) {if(x == "AAA" | x == "AA+" | x == "AA" | x == "AA-" | 
                                                                   x == "A+" | x == "A" | x == "A-") {x <- 3} 
                                                               else if (x == "BBB+" | x == "BBB" | x == "BBB-" | x == "BB+" | x == "BB" | x == "BB-" | 
 x == "B+" | x == "B" | x == "B-") {x <- 2} else if (x == "CCC+" | x == "CCC" | x == "CC+" | x == "CC" | x == "CC-" | 
                                                        x == "C+" | x == "C" | x == "C-") {x <- 1}}))
####################################
#Removing missing values
credit_df <- na.omit(credit_df1)
#######################################

####Correlation among variables

set.seed(7)
correlationMatrix <- cor(credit_df[,1:123])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
###############################

#########Variable selection based on importance rate
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
