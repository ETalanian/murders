library(tidyverse)
murders <- read_csv('data/murders.csv')
murders <- murders %>% mutate(region= factor(region), rate = total/population*10^5)
save(murders, file= 'rda/murders.rda')



system.file("extdata",package="dslabs")
path <- system.file("extdata",package='dslabs')
list.files(path)
filename <- 'murders.csv'
fullpath <- file.path(path, filename)
fullpath
file.copy(fullpath, getwd())
