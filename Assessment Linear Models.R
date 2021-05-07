library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

fit <- Teams_small %>%
  mutate(R = R/G) %>%
  lm(avg_attendance ~ R, data=.)
coef(fit)

fit <- Teams_small %>%
  mutate(HR = HR/G) %>%
  lm(avg_attendance ~ HR, data=.)
tidy(fit, conf.int = TRUE)


fit <- Teams_small %>%
  lm(avg_attendance ~ W, data=.)
tidy(fit, conf.int = TRUE)

fit <- Teams_small %>%
  lm(avg_attendance ~ yearID, data=.)
tidy(fit, conf.int = TRUE)

fit <- Teams_small %>%
  mutate(R = R/G) %>%
  lm(W ~ R, data=.)
tidy(fit, conf.int = TRUE)

fit <- Teams_small %>%
  mutate(HR = HR/G) %>%
  lm(W ~ HR, data=.)
tidy(fit, conf.int = TRUE)

df = Teams_small %>%
  mutate(R = R/G,
         HR = HR/G)

cor(df$W, df$R)
cor(df$W, df$HR)

new <- Teams_small %>%
  mutate(W = round(W/10), R = R/G) %>%
  filter(W %in% 5:10) %>% 
  ggplot() + 
  stat_qq(aes(sample=R)) + 
  facet_wrap(~W)
new

new <- Teams_small %>%
  mutate(W = round(W/10), HR = HR/G) %>%
  filter(W %in% 5:10) %>% 
  ggplot() + 
  stat_qq(aes(sample=HR)) + 
  facet_wrap(~W)
new

new5 <- Teams_small %>%
  mutate(W = round(W/10), R = R/G) %>%
  filter(W == 5) %>%
  lm(W ~ R, data=.)
new6 <- Teams_small %>%
  mutate(W = round(W/10), R = R/G) %>%
  filter(W == 6) %>%
  lm(W ~ R, data=.)
new7 <- Teams_small %>%
  mutate(W = round(W/10), R = R/G) %>%
  filter(W == 7) %>%
  lm(W ~ R, data=.)
new8 <- Teams_small %>%
  mutate(W = round(W/10), R = R/G) %>%
  filter(W == 8) %>%
  lm(W ~ R, data=.)
new9 <- Teams_small %>%
  mutate(W = round(W/10), R = R/G) %>%
  filter(W == 9) %>%
  lm(W ~ R, data=.)
new10 <- Teams_small %>%
  mutate(W = round(W/10), R = R/G) %>%
  filter(W == 10) %>%
  lm(W ~ R, data=.)
new5
new6
new7
new8
new9
new10

new5 <- Teams_small %>%
  mutate(W = round(W/10), HR = HR/G) %>%
  filter(W == 5) %>%
  lm(W ~ HR, data=.)
new6 <- Teams_small %>%
  mutate(W = round(W/10), HR = HR/G) %>%
  filter(W == 6) %>%
  lm(W ~ HR, data=.)
new7 <- Teams_small %>%
  mutate(W = round(W/10), HR = HR/G) %>%
  filter(W == 7) %>%
  lm(W ~ HR, data=.)
new8 <- Teams_small %>%
  mutate(W = round(W/10), HR = HR/G) %>%
  filter(W == 8) %>%
  lm(W ~ HR, data=.)
new9 <- Teams_small %>%
  mutate(W = round(W/10), HR = HR/G) %>%
  filter(W == 9) %>%
  lm(W ~ HR, data=.)
new10 <- Teams_small %>%
  mutate(W = round(W/10), HR = HR/G) %>%
  filter(W == 10) %>%
  lm(W ~ HR, data=.)
new5
new6
new7
new8
new9
new10
