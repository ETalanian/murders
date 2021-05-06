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


#Sophomore Slumps
#Create a table with PlayerID, Names, and Position
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)
playerInfo
#Clean up table
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by='playerID') %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by='playerID') %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P") #no pitchers allowed >:(
ROY
#Clean more to only keep rookie and sophomores
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID ==min(yearID), "rookie", "sophomore")) %>%
  filter(n()==2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)
ROY
#Clean up one last time to group the same person across 2 years
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY


two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(-playerID)
two_years
#sort by worst performers
arrange(two_years, `2013`)
#disproving the 'sophomore slump'
two_years %>% ggplot(aes(`2013`, `2014`)) + geom_point()
summarize(two_years, cor(`2013`,`2014`))
#The main reason stats go down by an average of 8% is due to polling from the top% in year 1

##Measurement Error Models
#Ex: Galileo dicovering the velocity of a falling object
falling_object <- rfalling_object()
falling_object %>%
  ggplot(aes(time, observed_distance)) + 
  geom_point() +
  ylab("Distance in Meters") + 
  xlab("Time in Seconds")

fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)
#check if the estimated parabola fits the data
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")
tidy(fit, conf.int = TRUE)



#assessment scratchpad
fit <- Teams %>%
  filter(yearID %in% 2011) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data=.)

tidy(fit, conf.int = TRUE)

res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")
