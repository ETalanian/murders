install.packages("HistData")
library(Lahman)
library(tidyverse)
library(dslabs)
library(HistData)
ds_theme_set()
data("GaltonFamilies")

#HRs vs Wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
#SBs vs Wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_Per_Game = R/ G) %>%
  ggplot(aes(SB_per_game, R_Per_Game)) +
  geom_point(alpha = .5)
#BBs vs Wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
#ABs vs Wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
#Es vs Wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(E_per_game = E / G, R_per_game = R / G) %>%
  ggplot(aes(E_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
#Triples vs Doubles
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(T_per_game = X3B / G, D_per_game = X2B / G) %>%
  ggplot(aes(T_per_game, D_per_game)) + 
  geom_point(alpha = 0.5)


galton_heights <- GaltonFamilies %>%
  filter(childNum==1 & gender=="male") %>%
  select(father,childHeight) %>%
  rename(son=childHeight)
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))
galton_heights %>% ggplot(aes(father, son)) +
  geom_point(alpha=.5)
galton_heights %>% summarise(cor(father,son))

set.seed(0) #sync for academia

#Sample correlation via RNG
R <- sample_n(galton_heights, 25, replace=TRUE) %>%
  summarise(cor(father,son))
R
#Monte Carlo it
B <- 1000
N <- 25
R <- replicate(B,{
  sample_n(galton_heights, N, replace=TRUE) %>%
    summarise(rr=cor(father,son)) %>% .$r
    })
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = .05, color = "black")
mean(R)
sd(R)

#QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))
