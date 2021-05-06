library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

Teams_small %>%
  ggplot(aes(R, avg_attendance))+
  geom_point()+
  geom_smooth(method = "lm")

fit <- Teams_small %>%
  lm(avg_attendance ~ R + HR, data=.)
tidy(fit, conf.int = TRUE)
