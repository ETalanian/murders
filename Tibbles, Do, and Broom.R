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

#Differences between Tibbles and Data Frames
#1. Tibbles display in a more readable format
Teams
as_tibble(Teams)
#2. Subsets of data frames are not always data frames
#   Subsets of tibbles are always tibbles
class(Teams[,20])
class(as_tibble(Teams)[,20])
##If you want the original vector that defines a column in a tibble, you use the accessor $
class(as_tibble(Teams)$HR)
#Tibbles give a warning if you try to access a column that does not exist
#data frames do not
Teams$hr
as_tibble(Teams)$hr
#3. Tibbles can have complex entries
#   Data frames can only accept a vector of numbers, strings, or boolean in its columns
tibble(id = c(1,2,3), func = c(mean,median,sd))
#4. Tibbles can be grouped
#group_by(tbl) returns a grouped tibble, this works with Tidyverse

