library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

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
