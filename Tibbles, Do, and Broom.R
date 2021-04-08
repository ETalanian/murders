install.packages("HistData")
library(Lahman)
library(tidyverse)
library(dslabs)
library(HistData)
library(dplyr)
ds_theme_set()

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)


dat %>%
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

#by eye, we see the slopes are similar, and the differences can be explained by random variation
#lm() provides information to construct confidence intervals for each slope

#DO NOT use lm() with group_by()
#These come from differnt packages, and lm() does not know how to factor in group_by() into it's algorithm

dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

#functions like group_by() and summarize() always return tbl (tibble) data frames
#group_by() returns a grouped tibble

#manipulation verbs like select(), filter(), mutate(), and arrange() preserve the class of the input

