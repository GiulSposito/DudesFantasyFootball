library(tidyverse)
library(glue)

.week <- 8
sim <- readRDS(glue("./data/simulation_v5_week{.week}_final.rds"))

players <- sim$players_stats %>% 
  select(playerId, name, position, weekPts, smallImageUrl) %>% 
  unnest(weekPts) %>% 
  filter(week==.week) %>% 
  select(-week, -weekPts) %>% 
  rename(seasonPts = weekSeasonPts) 

teams <- sim$teams %>% 
  unnest(rosters) %>% 
  select(playerId, teamId, team=name, imageUrl)

top_players <- players %>% 
  left_join(teams, by="playerId") %>% 
  group_by(position) %>% 
  top_n(5, seasonPts) %>% 
  arrange(position, desc(seasonPts)) %>% 
  ungroup()

# rank for position

top_players %>% 
  filter(position=="QB")
