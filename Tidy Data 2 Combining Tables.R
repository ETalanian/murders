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

#unlike intersect and union, this function is not symmetric
setdiff(1:10,6:15)
setdiff(6:15,1:10)

