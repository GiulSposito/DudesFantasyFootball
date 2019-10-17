library(tidyverse)

curr.week <- 5

points.projection <- readRDS("./data/points_projection.rds")
players.points <- readRDS("./data/players_points.rds")

error.dist <- players.points %>% 
  select(week, id, points) %>% 
  inner_join(points.projection, by = c("week", "id")) %>% 
  mutate( error = points - pts.proj ) %>% 
  filter( week < curr.week ) %>% 
  select( id, data_src, error )
  

unique(error.dist$week)
unique(.pts.proj$week)

bind_rows(
points.projection %>% 
  filter(week==curr.week) %>% 
  inner_join(error.dist, by = c("data_src", "id")) %>% 
  mutate( error.proj = error + pts.proj ) %>% 
  select(week, pos, data_src, id, pts.proj=error.proj, season) %>% 
  mutate( type = "error.dist")
,
points.projection %>% 
  filter(week==curr.week) %>% 
  mutate( type = "site.proj" )
) %>% 
  arrange(id, data_src, type) %>% 
  head(100) %>% 
  View()
