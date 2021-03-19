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


conditional_avg <- galton_heights%>% filter(round(father)==72) %>%
  summarize(avg = mean(son)) %>% .$avg
conditional_avg

galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot()+
  geom_point()
galton_heights %>%
  mutate(father=round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) + 
  geom_point()

mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = .5)+
  geom_abline(intercept = 0, slope = r)

galton_heights%>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha=.5)+
  geom_abline(intercept = 0, slope=r)

galton_heights %>%
  mutate(z_father = round((father-mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() + 
  stat_qq(aes(sample=son)) + 
  facet_wrap(~z_father)

#Assessment Scratchpad
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

head(female_heights)
mu_x <- mean(female_heights$mother)
mu_x
mu_y <- mean(female_heights$daughter)
mu_y
s_x <- sd(female_heights$mother)
s_x
s_y <- sd(female_heights$daughter)
s_y
r <- cor(female_heights$mother, female_heights$daughter)
r
m <- r * s_y/s_x
m
b <- mu_y - m*mu_x
b
