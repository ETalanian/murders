# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Machine Learning Basics

## Basics of Evaluating Machine Learning Algorithms Comprehension Check

### Caret package, training and test sets, and overall accuracy

library(caret)
library(dslabs)

data(heights)

#categorical outcome y|x
y <- heights$sex
x <- heights$height
#set seed for controlled rng variability
set.seed(2)
#split your data randomly in half to create your training data (Q&A) and testing data (Q&?) 
#`times` defines how many random samples of indexes to return
#`p`  is the proportion of the index represented
#`list` decides if you want indexes to be returned as a list or not
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
#formally split the data
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
#The most basic machine learning, 50/50 guessing
y_hat <- sample(c("Male", "Female"),length(test_index), replace = TRUE)
#Caret package recommends that categorical outcomes be coded as factors
y_hat <- sample(c("Male", "Female"),length(test_index), replace = TRUE) %>% 
     factor(levels = levels(test_set$sex))
#Overall proportion that is predicted correctly (overall accuracy)
mean(y_hat == test_set$sex) #Accuracy is ~50%, because we're making 50/50 guesses

#Check data to see that, on average, males are slightly taller than females
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
#predict male if height is within two standard deviations from the average male (62")
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat) #Accuracy goes from 50% to 80%
#examine the accuracy we obtain with 10 different cutoffs and peck the best one
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
          factor(levels = levels(test_set$sex))
     mean(y_hat == train_set$sex)
})
#plot the accuracy on the training set
data.frame(cutoff, accuracy) %>% 
     ggplot(aes(cutoff, accuracy)) + 
     geom_point() + 
     geom_line() 

max(accuracy) #83.6%, much higher than 50%

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff #Max Accuracy with a cutoff of 64"
#Check cutoff on test set
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
     factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex) #81.7%

### Confusion Matrix
#Tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
#In practice, we get very different accuracy for males (93.3%) and females (42.0%)
test_set %>% 
     mutate(y_hat = y_hat) %>%
     group_by(sex) %>% 
     summarize(accuracy = mean(y_hat == sex))
#this error is due, in part, to prevalence.  77.3% of the data set is male
prev <- mean(y == "Male")
prev
#sensitivity is defined as 'the ability of an algorithm to predict a positive outcome when the actual outcome is positive'
#specificity is defined as 'the ability of an algorithm to not predict the positive, when the actual outcome is not positive'
mat <- matrix(c("True positives (TP)", "False negatives (FN)", 
                "False positives (FP)", "True negatives (TN)"), 2, 2)
colnames(mat) <- c("Actually Positive", "Actually Negative")
rownames(mat) <- c("Predicted positve", "Predicted negative")
as.data.frame(mat) %>% knitr::kable()

confusionMatrix(data = y_hat, reference = test_set$sex)

### Balanced accuracy and F1 score
#One metric that is valued over overall accuracy is the average of sensitivity and specificity, referred to as 'balanced accuracy'
#Because sensitivity and specificity are rates, it's appropriate to compute the harmonic average as an 'F1-score':
#        1 / (0.5 x ( (1/recall) + (1/precision) ))
#or      2 x ( (precision x recall) /  (precision + recall) )

#Example of Sensitivity vs Specificity
#Failing to predict a plane will crash (specificity) is far more costly than grounding a plane you incorrectly thought would crash

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
          factor(levels = levels(test_set$sex))
     F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
     ggplot(aes(cutoff, F_1)) + 
     geom_point() + 
     geom_line()
#61%
max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
     factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)
#Takes height, predicts female if you're 66 inches or shorter.


### Prevalence matters in practice

### ROC and precision-recall curves

p <- 0.9
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
     factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
     y_hat <- 
          sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Guessing",
          FPR = 1 - specificity(y_hat, test_set$sex),
          TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Height cutoff",
          FPR = 1-specificity(y_hat, test_set$sex),
          TPR = sensitivity(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
     ggplot(aes(FPR, TPR, color = method)) +
     geom_line() +
     geom_point() +
     xlab("1 - Specificity") +
     ylab("Sensitivity")

map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Height cutoff",
          cutoff = x, 
          FPR = 1-specificity(y_hat, test_set$sex),
          TPR = sensitivity(y_hat, test_set$sex))
}) %>%
     ggplot(aes(FPR, TPR, label = cutoff)) +
     geom_line() +
     geom_point() +
     geom_text(nudge_y = 0.01)

guessing <- map_df(probs, function(p){
     y_hat <- sample(c("Male", "Female"), length(test_index), 
                     replace = TRUE, prob=c(p, 1-p)) %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Guess",
          recall = sensitivity(y_hat, test_set$sex),
          precision = precision(y_hat, test_set$sex))
})
height_cutoff <- map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Height cutoff",
          recall = sensitivity(y_hat, test_set$sex),
          precision = precision(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
     ggplot(aes(recall, precision, color = method)) +
     geom_line() +
     geom_point()

guessing <- map_df(probs, function(p){
     y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                     prob=c(p, 1-p)) %>% 
          factor(levels = c("Male", "Female"))
     list(method = "Guess",
          recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
          precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
height_cutoff <- map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Male", "Female"))
     list(method = "Height cutoff",
          recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
          precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
     ggplot(aes(recall, precision, color = method)) +
     geom_line() +
     geom_point()
