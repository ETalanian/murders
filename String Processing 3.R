library(dslabs)
library(tidyverse)
library(dslabs)
library(stringr)
library(rvest)
library(purrr)

data(reported_heights)

s <- c("5'10", "6'1")
tab <- data.frame(x=s)
tab %>% separate(x, c("feet", "inches"), sep="'")
s <- c("5'10", "6'1\"", "5'8inches")
tab <- data.frame(x=s)
tab %>% separate(x, c("feet", "inches"), sep="'", fill = "right")
tab %>% extract(x,c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")


#Read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

#separate the stings into values
x <- str_split(lines, ",")
head(x)
#remove column name row
col_names <- x[[1]]
x <- x[-1]

#extract first element of list entry using `purrr` package
map(x, function(y) y[1]) %>% head()
#shortcut
map(x,1) %>% head()

# extract columns 1-5 as characters, then convert to proper format
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

#more efficient use of purrr
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>%
  as.data.frame()
head(dat)

#without purrr
x <- str_split(lines, ",", simplify = TRUE)
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>% 
  mutate_all(parse_guess)


#Recoding
data("gapminder")
gapminder %>%
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
#some country names are too long, so we cut them down
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
#record works like ctrl+h





##Assessment 2 scratchpad:
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

head(polls)

names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls <- polls[str_detect(polls$remain, "%"), -9]
nrow(polls)
str_replace(polls$undecided, "N/A", "0")

temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
head(temp)
