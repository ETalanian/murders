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


