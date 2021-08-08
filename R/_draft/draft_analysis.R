library(tidyverse)
library(ffanalytics)
library(yaml)

draft_rosters <- readRDS("./data/drafted_teams_rosters.rds")
draft_proj <- readRDS("./data/drafted_season_projection.rds")

scrap <- readRDS("./data/scrap_season.rds")

# PROJECT FANTASY POINTS
proj_table  <- projections_table(scrap, read_yaml("./config/score_settings.yml"))

sim <- readRDS("./data/simulation_v5_week16_final.rds")

pstat <- sim$players_stats %>% 
  select(playerId, name, position, weekPts) %>% 
  unnest(weekPts) %>% 
  filter(week==max(week))

dt <- proj_table %>% 
  mutate( id=as.integer(id) ) %>% 
  inner_join(select(sim$players_id, id, nfl_id), by="id") %>% 
  inner_join(pstat, by=c("nfl_id"="playerId")) %>% 
  filter(avg_type=="weighted") %>% 
  select(playerId=nfl_id, name, position, proj=points, points=weekSeasonPts)



dt %>% 
   %>% 
  arrange(desc(points)) %>% 
  top_n(9*14,points) %>% 
  ggplot(aes(x=proj, y=points)) +
  geom_abline(slope=1, type="dashed") +
  geom_point() +
  theme_minimal()


draft_rosters %>% 
  unnest(rosters) %>% 
  select(teamId, team=name, playerId) %>% 
  inner_join(dt, by="playerId") %>% 
  ggplot(aes(x=proj, y=points, color=team)) +
  geom_abline(slope=1, type="dashed") +
  geom_point() +
  theme_minimal()

