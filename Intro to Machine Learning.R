#Y = Outcomes
#X's (X1 - Xp) = Features (or predictors, or covariates)

#Prediction problems can be Categorical or Continuous
#For Categorical outcomes, Y can be any of k classes
#ie k in a digit reader = 10 (1,2,3,4,5,6,7,8,9,0)
#ie k in speach detection = All posisble words
#ie k in a spam filter = 2 (Spam | Not Spam)
#in this course, k = 1 through K, and binary k = 0,1
#When the outcome is categorical, we refer to the machine-learning task as classification.
#our predictions will be categorical, just like our outcomes, and they will either be correct or incorrect.
#when the outcome is continuous, we'll refer to the task as prediction, which will be measured in error, instead of right/wrong.


#Yi = an outcome for observation or index i.
#We use boldface X_i for  to distinguish the vector of predictors from the individual predictors Xi,1....Xi,784.
#When referring to an arbitrary set of features and outcomes, we drop the index i and use Y and bold X.
#Uppercase is used to refer to variables because we think of predictors as random variables.
#Lowercase is used to denote observed values. For example, X = x.
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr) 
data("heights")

y <- heights$sex #categorical outcome
x <- heights$height

#caret's createDataPartition is used to generate indexes for randomly splitting data into training and test sets:
#      The argument `times` is used to define how many random samples of indexes to return
#      the argument `p` is used to define what proportion of the index represented and the argument 
#           `list` is used to decide if you want indexes to be returned as a list or not.

set.seed(2)
test_index <- createDataPartition(y, times = 1, p=0.5, list=FALSE)
train_set <- heights[-test_index,]
test_set <- heights[test_index,]

#start basic
y_hat <- sample(c("Male","Female"),
                length(test_index), replace=TRUE)
#code the categorical outcome as a factor
y_hat <- sample(c("Male","Female"),
                length(test_index), replace=TRUE) %>%
  factor(levels = levels(test_set$sex))
#compute overall accuracy
mean(y_hat == test_set$sex)
#52.38%
#Exploratory data analysis suggests we can do better because, on average, males are slightly taller than females
heights %>% group_by(sex) %>%
  summarize(mean(height), sd(height))
#predict Male is the height is within 2 standard deviations from the average male
y_hat <- ifelse(x>62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y==y_hat)
#79.333%
#The above line used a cutoff of 62 inches, but we can examine the accuracy obtained for other cutoffs and choose the best
#ONLY PICK THE BEST VALUE ON THE TRAINING SET!!
#Create 10 different cutoffs
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
#plot the results
data.frame(cutoff,accuracy)%>%
  ggplot(aes(cutoff,accuracy)) + 
  geom_point() +
  geom_line()
#or make the computer do the thinking
max(accuracy)
#83.6%
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
#64
#Now we test this cutoff on the test set
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)
#81.7%



