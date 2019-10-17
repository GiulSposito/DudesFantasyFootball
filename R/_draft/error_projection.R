library(tidyverse)

curr.week <- 5

points.projection <- readRDS("./data/points_projection.rds")
players.points <- readRDS("./data/players_points.rds")

error.dist <- players.points %>% 
  select(week, id, points) %>% 
  inner_join(points.projection) %>% 
  mutate( error = points - pts.proj ) %>% 
  filter( week < curr.week )

unique(error.dist$week)
unique(.pts.proj$week)

.pts.proj %>% 
  unnest(projection) %>% 
  inner_join(error.dist, by = c("id", "pos", "data_src", "pts.proj")) %>% 
  arrange(week.x, desc(id), data_src)
