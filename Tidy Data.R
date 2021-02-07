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

##Tidy Data:  Each row represents one observation and the columns represent the different variables that we have data on for those observations
#import and inspect example of original Gapminder data in wide format
path <- system.file('extdata',package='dslabs')
filename <- file.path(path, 'fertility-two-countries-example.csv')
wide_data <- read_csv(filename)
select(wide_data,country,'1960':'1967')
##wide format:  Each row includes several observations, and one of the variables is stored in the header

new_tidy_data <- wide_data %>% 
  gather(year, fertility, '1960':'2015')
head(new_tidy_data)

new_tidy_data <- wide_data %>%
  gather(year,fertility,-country)
head(new_tidy_data)

class(tidy_data$year)
class(new_tidy_data$year)

#fix year from char to int
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

new_tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#sometimes we need data to be WideHard and not tiny, `spread()` is used for this.
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, '1960':'1967')
