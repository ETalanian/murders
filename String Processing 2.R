library(dslabs)
library(tidyverse)
library(dslabs)
library(stringr)
library(rvest)

data(reported_heights)

class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 

#Find all entries with 'cm' in them
str_subset(reported_heights$height, 'cm')

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

#\d = digit (0,1,2,3,4,5,6,7,8,9)
yes <- c('5','6',"5'10",'5 feet',"4'11")
no <- c("",'.','Five','six')
s <- c(yes,no)
#use \\d so r doesn't just cancel the d
pattern <- "\\d"
str_detect(s,pattern)

str_view(s,pattern)
str_view_all(s,pattern)
