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