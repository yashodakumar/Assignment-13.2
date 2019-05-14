library(data.table)
library(foreach)
library(readr)
library(dplyr)
library(corrplot)
library(car)
library(MASS)
library(ggplot2)
library(reshape2)
library(forecast)

setwd("C:/Users/Tyke/Downloads/BlogFeedback")
getwd()

blogData_train <- read_csv("C:/Users/Tyke/Downloads/BlogFeedback/blogData_train.csv")
View(blogData_train)

# retrieve filenames of test sets
test_filenames = list.files(pattern = "blogData_test")

# load and combine dataset
train = read_csv("blogData_train.csv")
fbtest = foreach(i = 1:length(test_filenames), .combine = rbind) %do% {
  temp = fread(test_filenames[i], header = FALSE)
}


# Giving names to the train and test data set
colnames(blogData_train) <- c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
                              "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
                              "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
                              "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
                              "basetue","basewed","basethu","basefri","basesat","target")
colnames(fbtest) <- c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
                      "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
                      "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
                      "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
                      "basetue","basewed","basethu","basefri","basesat","target","V55","V56","V57","V58",
                      "V55","V56","V57","V58","V55","V56","V57","V58")

dim(blogData_train)
dim(fbtest) 
View(blogData_train)
View(fbtest)
str(blogData_train)
str(fbtest)

train <- blogData_train
test <- fbtest
head(train)
head(test)

# cleaning data by constructing single collumn for post publish day 
train$pubday<- ifelse(train$sun ==1, 1, ifelse(train$mon ==1, 2, ifelse(train$tue ==1, 3,
                                                                        ifelse(train$wed ==1, 4, ifelse(train$thu ==1, 5, ifelse(train$fri ==1, 6,
                                                                                                                                 ifelse(train$sat ==1, 7, NA)))))))
# cleaning data by constructing single collumn for base day
train$baseday<- ifelse(train$basesun ==1, 1, ifelse(train$basemon ==1, 2, ifelse(train$basetue ==1, 3,
                                                                                 ifelse(train$basewed ==1, 4, ifelse(train$basethu ==1, 5,
                                                                                                                     ifelse(train$basefri ==1, 6, ifelse(train$basesat ==1, 7, NA)))))))

#a. Create a linear regression model to predict the number of comments
#in the next 24 hours (relative to base time).
library(MASS)

final_model <- lm(target ~ checkin + talking + d5 + d6 + d7 + d8 + d9 + d10 + d11 + 
                    d12 + d13 + d16 + d17 + d19 + d20 + d21 + d22 + d23 + d24 + 
                    cc1 + cc2 + cc3 + cc4 + basetime + postshre + Hhrs + wed + 
                    thu + fri + basemon + basewed, data = train)
summary(final_model)


#b. Fine tune the model and represent important features Visualize the
#dataset and make inferences from that.

final_model <- lm(target ~ talking + d5 + d7 + d8 + d10 + d11 + 
                    d12 + d13 + d16 + d17 + d19 + d20 + d22 + d23 + 
                    cc1 + cc2 + cc3 + cc4 + basetime + postshre + Hhrs, data = train)
summary(final_model)

prediction <- predict(final_model, test)
predicted <- data.frame(cbind(actuals = test$target, prediction = prediction))
predicted$prediction <- ifelse(prediction<0, 0, round(prediction,0))
cor(predicted)
View(predicted)

#c. Interpret the summary of the linear model.

summary(final_model)

#d. Report the test accuracy vs. the training accuracy

# test accuracy
round(accuracy(predicted$prediction,predicted$actuals),3)

prediction <- predict(final_model, test)
predicted <- data.frame(cbind(actuals = test$target, prediction = prediction))
predicted$prediction <- ifelse(prediction<0, 0, round(prediction,0))

min_max_accuracy <- mean(apply(predicted, 1, min) / apply(predicted, 1, max)) 
min_max_accuracy 

# training accuracy
round(accuracy(predicted$prediction,predicted$actuals),3)

prediction <- predict(final_model, train)
predicted <- data.frame(cbind(actuals = train$target, prediction = prediction))
predicted$prediction <- ifelse(prediction<0, 0, round(prediction, 0))
min_max_accuracy <- mean(apply(predicted, 1, min) / apply(predicted, 1, max)) 
min_max_accuracy 