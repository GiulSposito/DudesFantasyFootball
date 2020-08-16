library(tidyverse)
library(glue)

.week <- 5

pproj <- readRDS(glue("./data/week{.week}_players_projections.rds")) %>% 
  select(id, nfl_id, fantasy.team)

ppts <- readRDS("./data/players_points.rds") %>% 
  filter(week==.week)

players <- inner_join(pproj, ppts, by = c("id", "nfl_id")) %>% 
  select(-rosterSlots) %>% 
  filter(fantasy.team!="*FreeAgent")


ppts <- readRDS("./data/players_points.rds") %>%
  filter(week %in% 1:5) %>%
  group_by(id, nfl_id) %>%
  summarise(
    pts.total = sum(points),
    points = mean(points),
    pts.sd  = sd(points),
    count   = n()
  ) %>%
  ungroup() %>%
  mutate( week=.week) %>%
  inner_join(select(readRDS("./data/players_points.rds"),-points))
  
st01 <- tibble(
  pos=c("QB","RB","WR","TE","K","DEF"),
  qtd=c(1,2,2,1,1,1)
) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      filter(position==.x$pos) %>%
      top_n(.x$qtd, points)
  }, .players=players)

starters <- players %>% 
  filter(position %in% c("WR","RB")) %>% 
  anti_join(st01) %>% 
  top_n(1, points) %>% 
  bind_rows(st01,.)

ideal_team <- starters %>% 
  mutate(fullName = paste0(firstName, " ", lastName)) %>% 
  select(fullName, position, team, points, fantasy.team, injuryStatus)

ideal_team

sum(ideal_team$points)

