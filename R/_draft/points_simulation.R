library(tidyverse)

# carregando pontos e projeções
points <- readRDS("./data/players_points.rds")
projections <- readRDS("./data/players_projections.rds") %>% 
  rename(pts.proj = points) 
  


# montando dataset de comparação pontos e projecao
points %>% 
  inner_join(projections, by = c("src_id", "season", "week")) %>%
  select(id, player, position, season, week, points, data_src, pts.proj) %>% 
  mutate( data_src = as.factor(data_src),
          position = as.factor(position),
          error = (pts.proj-points) ) %>% 
  filter( week < 7 ) %>% 
  select(id, data_src, position, error) -> error.dist

projections %>% 
  filter( week == 7 )
  