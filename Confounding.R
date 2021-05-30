library(tidyverse)
library(dslabs)
library(Lahman)
library(tidytext)
N <- 25
g <- 1000000
#Generate 1m groups, each with 25 observations
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N*g), y= rnorm(N*g))
res <- sim_data %>%
  group_by(group) %>%
  summarize(r = cor(x,y)) %>%
  arrange(desc(r))
res

#Graph shows the data is corelated, but we know it's not causational since we made this random data
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

#shown in a different way
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = .1, color = 'black')
#Perfect distribution of random values

#perform regression and interpret the p-value to incorrectly show significance
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data=.)))



#Correlation is Not Causation: Outliers
set.seed(1)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])
##tibble(x,y) %>% ggplot(aes(x,y)) + geom_point((alpha = 0.5))
#Plot shows the outlier
qplot(x, y, alpha = 0.5)
cor(x,y)
#9.88
cor(x[-23], y[-23])
#-0.001
qplot(x[-23], y[-23])
cor(rank(x), rank(y))
#0.0658
cor(x,y, method = 'spearman')
#0.0658



#Correlation is Not Causation: Confounders
#If X and Y are correlated, we call Z a confounder if changes in Z cause changes in both X and Y.

data("admissions")
admissions

admissions %>% group_by(gender) %>%
  summarize(percentage = round(sum(admitted*applicants)/sum(applicants),1))
admissions %>% group_by(gender) %>%
  summarize(total_admitted = round(sum(admitted/100*applicants)),
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>%
  do(tidy(chisq.test(.)))

admissions %>%
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()

admissions %>% group_by(gender) %>% summarize(average = mean(admitted))










