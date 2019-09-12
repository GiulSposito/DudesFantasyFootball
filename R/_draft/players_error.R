library(tidyverse)

sim <- readRDS("./data/week2_simulation_v3.rds")

proj <- readRDS("./data/points_projection.rds")
pts  <- readRDS("./data/players_points.rds")

cur.week <- 2

dtf <- pts %>% 
  rename(nfl_id=id.y) %>% 
  inner_join(proj, by=c("nfl_id"="id", "week", "season"))  %>% 
  select(id, nfl_id, season, week, pos, data_src, pts.proj, points )  %>% 
  filter(week!=cur.week) %>% 
  mutate(error=pts.proj-points)

dtf %>% 
  ggplot() +
  geom_point(aes(x=pts.proj, y=points, color=pos)) +
  facet_grid(pos~data_src, scales = "free") +
  geom_abline(intercept=0, slope=1, linetype="dashed") + 
  theme_minimal()

dtf %>% 
  group_by(data_src, pos) %>% 
  nest() %>% 
  mutate( model.glance = map(data, function(.d){
    lm(points~pts.proj, data=.d) %>% 
      broom::glance() %>% 
      return()
  })) %>% 
  unnest(model.glance) %>% 
  ggplot() +
  geom_point(aes(y=r.squared, x=sigma, color=pos)) +
  theme_minimal()



