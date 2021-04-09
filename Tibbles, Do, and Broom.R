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

#do() serves as a bridge between R functions, such as lm(), and the tidyverse
dat %>%
  group_by(HR)%>%
  do(fit = lm(R ~ BB, data=.))
#do() created a data frame with the first column being strata, 
#and a customly named column, which contains the result of the lm() call
#if this is not named, do() will return the actual output of lm(), not a data frame, which will error out
dat %>%
  group_by(HR)%>%
  do(lm(R ~ BB, data=.))
#Build a function that returns only what you want in the form of a data frame:
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2],
             se = summary(fit)$coefficient[2,2])
}
#If we do not name the output, which we can since we're already getting a data frame, we get the following:
dat %>%
  group_by(HR) %>%
  do(get_slope(.))
#If we do use a name, we get a complex tibble with a column having a data frame in each cell (not useful)
dat %>%
  group_by(HR)%>%
  do(slope = get_slope(.))

#If the data frame being returned has more than one row, they will be concatenated
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficient[,2])
}
dat %>%
  group_by(HR)%>%
  do(get_lse(.))
#this gives us the estimates of the slope and intercept, as well as the standard errors

#Broom has 3 main functions: tidy(), glance(), and augment()
library(broom)
#tidy() returns esitmates and related information as a data frame
fit <- lm(R ~ BB, data = dat)
fit
tidy(fit)
#add confidence intervals
tidy(fit, conf.int = TRUE)
#because this outcome is a data frame, we can use the output with do()
dat %>%
  group_by(HR)%>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE))
#Now we filter for the data we want
dat %>%
  group_by(HR)%>%
  do(tidy(lm(R ~ BB, data= .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)
#this table is very visualization-friendly
dat %>%
  group_by(HR)%>%
  do(tidy(lm(R ~ BB, data= .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y=estimate, ymin=conf.low, ymax=conf.high))+
  geom_errorbar() +
  geom_point()

#glance(): model-specific outcomes
#augment(): observation-specific outcomes
glance(fit)
augment(fit)


#Assessment Scratchpad
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

t <- galton %>% group_by(pair)
nrow(t %>% filter(pair == "father_daughter"))
nrow(t %>% filter(pair == "mother_son"))
fd <- t %>% filter(pair == "father_daughter")
fs <- t %>% filter(pair == "father_son")
md <- t %>% filter(pair == "mother_daughter")
ms <- t %>% filter(pair == "mother_son")

galton %>% 
  group_by(pair) %>% 
  summarize(cor = cor(parentHeight, childHeight))

