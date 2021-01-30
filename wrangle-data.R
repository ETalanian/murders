library(tidyverse)
library(readxl)
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
file.exists(filename)
dat <- read_csv(filename)
dat <- read_csv(fullpath)
head(dat)
