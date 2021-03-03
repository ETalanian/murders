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

#Character Classes
str_view(s,"[56]")
str_view(s,'[4-7]') #'[0-9]' == '\\d'
#[1-20] == 1 through 2 & 0
#[a-z] == abc...xyz
#[A-Z] == ABC...XYZ
#[a-zA-Z] == abc.........XYZ

#Anchors
# ^ <- Beginning of string
# $ <- End of string
#^\\d$ <- Start of the string, followed by 1 digit, followed by the end of the string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)
#2nd "1" doesn't show because it is " 1" not "1"
#\\d{1,2} <- One OR Two digits
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)
#'optimal' parse
pattern <- "^[4-7]'\\d{1,2}\"$"
          #Start, 4-7 feet, 1-2 digits for inches
yes <- c("5'7\"", "6'2\"",  "5'12\"", "4'")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)
#This parse fails on flat feet.


#Search and Replace with Regex
# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

#Inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")
#inch marker at end is irrelevant
pattern <- "^[4-7]'\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot","'") %>%
  str_replace("inches|in|''|\"", "") %>% #kill off all inches because now irrelevant
  str_detect(pattern) %>%
  sum
#\\s <- space
pattern2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern2)
#* <- zero or more instances of the previous character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")
#? <- zero or one instances of previous character
#+ <- one or more instances of previous character
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_detect(pattern) %>% 
  sum()

#Groups with Regex
pattern_wo_groups <- "^[4-7],\\d*$"
pattern_w_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_wo_groups)
str_detect(s, pattern_w_groups)
str_match(s,pattern_w_groups)
str_extract(s, pattern_w_groups)
#The regex special char for the i-th group is \\i
#\\1 is the value extracted from the first group
#\\2 is the value extracted from the second group
#etc

#replace a comma with a period ONLY when it is between 2 digits
pattern_w_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_w_groups, "\\1'\\2")

#holy mother of god let's go
pattern_w_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
#Translation:
#^ <- Start of string
#[4-7] <- on digit, either 4, 5, 6, or 7
#\\s* <- none or more white spaces
#[,\\.\\s+] <- feet symbol is either a comma, a period, or at least one space
#\\s* <- none or more white spaces
#\\d* <- none or more digits
#$ <- End of string
str_subset(problems, pattern_w_groups) %>% head
str_subset(problems, pattern_w_groups) %>% 
  str_replace(pattern_w_groups, "\\1'\\2") %>% head
#Only one error, 5'25

