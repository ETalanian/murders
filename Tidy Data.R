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

##gather(data_set, *sets name of columns that will hold variable in*, *sets column name for columns*, *columns to be gathered*)
##spread(data_set, *sets which variable will be used as the column names*, *which variable to use to fill out cells)

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



path <- system.file('extdata',package='dslabs')
filename <- file.path(path, 'life-expectancy-and-fertility-two-countries-example.csv')
raw_dat <- read_csv(filename)
select(raw_dat,1:5)

dat <- raw_dat %>% gather(key,value,-country)
head(dat)

#separate `year` and `type` via identifier _ 
dat %>% separate(key, c("year","variable_name"), "_")
#_ is the default separater, so we can just write
dat %>% separate(key, c('year','variable_name'))
#warning shows that "life_expectancy" breaks due to this
#split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right")
#split on FIRST underscore, but keep life_expectancy merged, then spread
dat %>% separate(key, c('year', 'variable_name'), sep='_', extra="merge") %>% spread(variable_name, value)
#separate then unite
dat %>% separate(key, c("year", 'first_variable_name','second_variable_name'), fill = "right") %>%
        unite(variable_name, first_variable_name, second_variable_name, sep='_')
#full tidy code
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

##“Key” should specify which column has the unique values that will be used as the column names,
## and “value” should specify which values will be spread across those columns.


co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide
co2_tidy <- gather(co2_wide, )

?gather
?separate