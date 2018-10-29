library(tidyverse)
library(plotly)

players <- readRDS("./data/players_points.rds")
rosters <- readRDS("./data/week8_simulation_v2.rds")

c("home","away") %>% 
  map(function(.prefix){
    rosters %>%
      select( starts_with(.prefix) ) %>%
      set_names(gsub(paste0(.prefix, "."), "", names(.))) %>% 
      select( teamId, name, roster ) %>% 
      mutate(
        roster = map(roster, function(.team){
          .team %>% 
            select(id, player=name)
        })
      ) %>% 
      unnest(roster)
  }) %>% 
  bind_rows() %>% 
  select(id, team=name) -> teams


players %>% 
  filter(points!=0, position=="WR", week>5) %>% 
  select(id, firstName, lastName, position, week, points) %>% 
  mutate(
    name = paste0(firstName, " ", lastName)
  ) %>% 
  group_by(id, name, position) %>% 
  top_n(2,week) %>% 
  summarise(
    pts.mean = mean(points, na.rm = T),
    pts.sd   = sd(points, na.rm=T )
  ) %>%
  ungroup() %>% 
  left_join(teams, by="id") %>% 
  mutate(
    team = case_when(
      is.na(team) ~ "*FA",
      TRUE ~ team
    ),
    team = as.factor(team)
  ) %>% 
  plot_ly(x=~pts.sd, y=~pts.mean, color=~team,
          type="scatter", mode="markers", 
          text=~paste("Name: ", name)) 
  
