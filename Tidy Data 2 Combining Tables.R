#import US murders data
library(tidyverse)
install.packages("ggrepel")
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

#import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

#join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by='state')
head(tab)

#plot votes v pop
tab %>% ggplot (aes(population/10^6, electoral_votes, label=abb))+
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2') +
  geom_smooth(method='lm', se=FALSE)

#make 2 smaller tables to demonstrate joins
tab1 <- slice(murders,1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

#experiment with different joins
left_join(tab1, tab2)
#left_join: Join matching values from y to x.
tab1 %>% left_join(tab2)
#right_join: Join matching values from x to y.
tab1 %>% right_join(tab2)
#inner_join: Jain data, retain only rows with matches.
inner_join(tab1, tab2)
#semi_join: Return rows of x that have a match in y.
#           Useful to see what will be joined.
semi_join(tab1, tab2)
#anti_join: Return rows of x that do NOT have a match in y.
#           Useful to see what will not be joined.
anti_join(tab1, tab2)
#full join: Join data, return all values, all rows
full_join(tab1,tab2)


#Binding functions do not try to match by a variable, but rather just combine data sets
bind_cols(a=1:3, b=4:6)
#Bind can also bind data frames
tab1 <- tab[,1:3]
tab2 <- tab[,4:6]
tab3 <- tab[,7:9]
new_tab <- bind_cols(tab1,tab2,tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1,tab2)

intersect(1:10, 6:15)
intersect(c('a','b','c'),c('b','c','d'))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1,tab2)

union(c('a','b','c'),c('b','c','d'))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1,tab2)

#unlike intersect and union, setdiff is not symmetric
setdiff(1:10,6:15)
setdiff(6:15,1:10)

tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1,tab2)
setdiff(tab2,tab1)

setequal(1:5,1:6)
setequal(1:5,5:1)
setequal(tab1, tab2)

#assessment
install.packages("Lahman")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

AwardsPlayers
AwardsPlayers %>% filter(yearID=='2016')


library(rvest) #rvest = harvest
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h
tab <- h %>% html_nodes('table')
tab <- tab[[2]]
tab
#turn tab into a data frame
tab <- tab %>% html_table
class(tab)
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

#assessment
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])
html_table(nodes[[4]])
tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
full_join(tab_1,tab_2)

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
class(h)
h
tab <- h %>% html_nodes('table')
tab <- read_html(url) %>% html_nodes('table') #same output
tab
length(tab)
head(html_table(tab, fill=TRUE))
