library(HistData)
data("GaltonFamilies")
library(tidyverse)
library(dslabs)
library(lubridate)
install.packages("gutenbergr")
library(gutenbergr)
install.packages("tidytext")
library(tidytext)
install.packages("textdata")
library(textdata)

options(digits = 4)

gutenberg_metadata

set.seed(1983)


galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
fatherh <- 
#compute RSS for any pair of values, b0 and b1, for heights
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
beta1 = seq(0,1,len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1,rss)) + geom_line()+ 
  geom_line(aes(beta1, rss), col=2)

#beta1 min reads 0.65, but this is for when beta0 is fixed at 25
#We don't know if (25,0.65) minimizes the equation across all pairs.

#We can obtain the LSE using the lm function
fit <- lm(son ~ father, data = galton_heights)
#   ~
#What we're predicting ~ What we're using to predict
fit
summary(fit)

#LSE are Random Variables
#To midigate randomization, we Monte Carlo Simulate
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N , replace = TRUE) %>%
    lm(son ~ father, data = .) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

sample_n(galton_heights, N, replace=TRUE) %>%
  lm(son ~ father, data = .) %>% summary

galton_heights %>% ggplot(aes(son, father)) + 
  geom_point() + 
  geom_smooth(method = "lm")

#predict() takes an lm object as input and returns a confidence prediction
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data = .))) %>%
  ggplot(aes(father, Y_hat)) + 
  geom_line()
#`predict` methodology
fit <- galton_heights %>% lm(son ~ father, data=.)
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)


#Assignment Scratchpad
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
beta1 = seq(0,1,len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1,rss)) + geom_line()+ 
  geom_line(aes(beta1, rss), col=2)

library(Lahman)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, BB_per_game = BB / G) %>%
  ggplot(aes(HR_per_game, BB_per_game)) + 
  geom_point(alpha = 0.5)
Teams %>% filter(yearID %in% 1961:2001) %>%
  summarise(cor(HR/G,BB/G))

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female_heights %>% ggplot(aes(mother, daughter)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")
female_heights %>% lm(mother ~ daughter, data=.)

