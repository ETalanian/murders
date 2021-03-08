library(tidyverse)
library(dslabs)
library(lubridate)

data("polls_us_election_2016")

#Date originally structured as date
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
#Date can be translated into a number (Days since Jan 1, 1970)
as.numeric(polls_us_election_2016$startdate) %>% head

#ggplot works with dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump))+
  geom_line()
#note that months are displaced

set.seed(2) #orient seed with class
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates
data.frame(date = days(dates),
           month = month(dates),
           day = day(dates),
           year = year(dates))
month(dates, label=TRUE)

#parse strings into dates
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

#match YYYY-MM-DD with ymd
#for data organized elsewise, use mdy, etc.
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

#get current time
Sys.time() #R-base code
now() #lubridate code
now("GMT") #see GMT time
OlsonNames() #Check listing of all available timezones in lubridate
now() %>% hour()
now() %>% minute()
now() %>% second()
##Side note - now() is a fantastic alternative for RNG.
x <- c("12:34:56")
hms(x) #hours/minutes/seconds
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)




##Assessment 1 scratchpad
#library(dslabs)
#library(lubridate)
options(digits = 3)    # 3 significant digits
data(brexit_polls)
head(brexit_polls)
#x <- brexit_polls %>% select(brexit_polls$startdate>"2016-04-01" & brexit_polls$startdate<"2016-04-31")
#x <- brexit_polls$startdate
#ymd(x)
#x <- as.numeric(brexit_polls$startdate)
#filter(x, 2016-04-01<=x & x<=2016-04-31)
#x
#ymd(x)
#sort(x)
x <- round_date(brexit_polls$enddate, unit="week")
x
y <- weekdays(brexit_polls$enddate)
y
length(y=="Monday")
length(y)
y <- table(y) #find highest occuring weekday
y
data(movielens)
movielens$timestamp
time <- as_datetime(movielens$timestamp)
time
sort(time)
data.frame(date = days(time),
           month = month(time),
           day = day(time),
           year = year(time),
           hour = hour(time))
year(time)
table(year(time)) #find highest occuring year
hour(time)
table(hour(time)) #find highest occuring hour

#Assessment #2 scratchpad
install.packages("gutenbergr")
library(gutenbergr)
install.packages("tidytext")
library(tidytext)
options(digits = 3)

gutenberg_metadata
str_which(gutenberg_metadata$title, "Pride and Prejudice") #question said to use str_detect() but this worked better for me
?gutenberg_works
gutenberg_works(title == "Pride and Prejudice", languages = "en")
book <- gutenberg_download(1342)
??tidytext
book
words1 <- tibble(book)
words1
words <- words1 %>% unnest_tokens(word, text)
words
words2 <- words %>% anti_join(stop_words)
words2
nrow(words2)
words3 <- words2 %>% filter(!str_detect(word, "\\d+"))
words3
book%>% unnest_tokens(word, text)  %>% anti_join(stop_words, by = 'word') %>% filter(!str_detect(word, "\\d+")) %>% dplyr::count(word) %>% filter(n>= 100) %>% arrange(desc(n))

install.packages("textdata")
library(textdata)
afinn <- get_sentiments("afinn")

