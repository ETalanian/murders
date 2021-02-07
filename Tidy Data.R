library(tidyverse)
library(dslabs)
data(gapminder)

#create and inspect a tidy data frame
tidy_data <- gapminder %>%
  filter(country %in% c('South Korea','Germany')) %>% 
  select(country, year, fertility)
head(tidy_data)

#plot data
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#import and inspect example of original Gapminder data in wide format
