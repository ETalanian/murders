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
