library(tidyverse)
install.packages("ggrepel")
library(ggrepel)
library(dslabs)
library(stringr)
library(rvest)

#Read raw murder data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

head(murders_raw)
#should be numbers, appears as characters
class(murders_raw$population)
class(murders_raw$total)

s <- '10"'
cat(s)
s <- "5'"
cat(s)
s <- '5\' 10"'
cat(s)


#stringr is included in `library(tidyverse)`
murders_raw$population[1:3]
#"4,853,875" "737,709"   "6,817,565"
#the following does not work because of the commas
as.numeric(murders_raw$population[1:3])
#The stringr package from the tidyverse includes a variety of string processing functions that begin with str_
#        and take the string as the first argument, which makes them compatible with the pipe.

commas <- function(x) any(str_detect(x, ','))
#murders_raw %>% summarise_all(funs(commas))
#Due to deprecated error, per the error's recomendation, I will use `list`
murders_raw %>% summarise_all(list(commas))
test_1 <- str_replace_all(murders_raw$population, ',', '')
test_1 <- as.numeric(test_1)
test_1
test_2 <- parse_number(murders_raw$population)
test_2
#let the computer do the heavy lifting
identical(test_1, test_2)

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
head(murders_new)
