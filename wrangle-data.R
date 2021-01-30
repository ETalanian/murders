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
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))
dat <- read_csv(filename)
dat <- read_csv(fullpath)
head(dat)


filename <- 'murders.csv'
dat3 <- read.csv(filename)
class(dat3)
class(dat3$abb)
class(dat3$region)


url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat4 <- read_csv(url)
download.file(url,'murders.csv')
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)
