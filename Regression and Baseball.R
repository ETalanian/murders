install.packages("HistData")
library(Lahman)
library(tidyverse)
library(dslabs)
library(HistData)
ds_theme_set()
data("GaltonFamilies")
library(dplyr)
library(broom)

#Yi = B0 + B1X1 + B2X2 + Ei
#Yi = Runs per Game
#X1 = Walks per Game
#X2 = Home Runs per Game

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data=.)
  
#`+` lets lm() know we have two predictor variables

tidy(fit, conf.int = TRUE)


#Assuming our variables are jointly normal...
      #'Jointly normal' means that if we pick any one of our variables and hold the other four as fixed
      #the relationship with the outcome is linear
      #and the slopes for this relationship do not depend on the four values that were held constant
#than a linear model for our data is:
#Yi = B0 + B1Xi1 + B2Xi2 + B3Xi3 + B4xi4 + B5Xi5 + Ei
#x1 = BB/G, X2 = Singles/G, X3 = Doubles/G, X4 = Triples/G, X5 = HR/G

#Find the least squared errors for the parameters
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR=HR/G,
         R=R/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data=.)
coefs <- tidy(fit, conf.int = TRUE)
coefs


Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR=HR/G,
         R=R/G) %>%
  mutate(R_hat = predict(fit, newdata=.))%>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

#Extract Plate Appearances per Game
pa_per_g <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_g = sum(AB+BB)/max(G)) %>%
  .$pa_per_g %>%
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_g,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

players %>% ggplot(aes(R_hat)) + 
  geom_histogram(binwidth = .5, color = "black")

#Now we build a team.  Things to consider
  #Position (most played (top_n))
  #Salary
  #Effectiveness
#add Salaries
players  <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")
#Add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))
#Name players for readability
players  <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by="playerID")
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>%
  top_n(10)
#Good players get paid well, as visualized by:
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() + 
  scale_x_log10()
