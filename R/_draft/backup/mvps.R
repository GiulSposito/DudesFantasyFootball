library(tidyverse)

pp <- readRDS("./data/players_points.rds")
players <- readRDS("./data/players.rds")

total.points <- pp %>% 
  select(id, nfl_id, name, position, week, points) %>% 
  group_by(id, nfl_id, name, position) %>% 
  summarise(points=sum(points, na.rm = T)) %>% 
  ungroup() %>% 
  inner_join(
    select(filter(pp, week==max(week)), id, nfl_id, team),
    by = c("id", "nfl_id")
  ) %>% 
  inner_join(
    select(players, playerId, imageUrl, smallImageUrl, largeImageUrl),
    by=c("nfl_id"="playerId")
  )

total.points %>% 
  group_by(position) %>% 
  top_n(3, points) %>% 
  arrange(desc(points))
