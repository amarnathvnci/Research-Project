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
