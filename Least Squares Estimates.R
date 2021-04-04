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
