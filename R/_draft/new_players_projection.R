library(tidyverse)
library(glue)


.week <- 9

# retorna um summary como um data.frame
summaryAsTibble <- . %>% summary() %>% as.list() %>% as.tibble()

quantileAsTibble <- function(x, probs){
  quantile(x, probs, na.rm = T) %>% 
    as.list() %>% 
    as.tibble() %>% 
    purrr::set_names(c("low","med","high")) %>% 
    return()
}

# historic fantasy points (nfl)
points <- readRDS("./data/players_points.rds")

# players projections (ffa)
pts.proj <- readRDS("./data/points_projection.rds")

# calcula distribuição do erro
errors <- points %>%
  filter(week!=.week) %>% 
  inner_join(pts.proj, by = c("id", "season", "week")) %>% 
  select( id, position, week, season, data_src, points, pts.proj ) %>% 
  mutate( error=points-pts.proj ) 

# calcula distribuição de erros por posicao e source
error.dist <- errors %>% 
  select(data_src, position, error) %>% 
  nest(error) %>% 
  mutate( error.dist = map(data, function(.data) pull(.data,error)) ) %>% 
  select(-data)

pts.proj %>%
  rename(position=pos) %>% 
  filter(week==.week) %>% 
  inner_join(error.dist, by = c("position", "data_src")) %>% 
  mutate(
    pts.dist = map2(pts.proj, error.dist, function(.pts,.dst) .pts+.dst),
    pts.dist = map(pts.dist, base::sample, size=200, replace=T) 
  ) %>% 
  select(-week) %>% 
  group_by(id, position) %>% 
  nest(pts.dist, .key="pts.dist") %>% 
  mutate( pts.dist = map(pts.dist, unlist),
          pts.dist.summ = map(pts.dist, summaryAsTibble),
          pts.range.80pct   = map(pts.dist, quantileAsTibble, probs=c(.125, .50, .875)) ) %>% 
  unnest(pts.dist.summ, .sep = ".") %>% 
  unnest(pts.range.80pct, .sep=".")


