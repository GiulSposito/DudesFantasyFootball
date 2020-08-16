library(tidyverse)
library(glue)

cur.week <- 4

sim <- readRDS(glue("./data/week{cur.week}_simulation_v3.rds"))
proj <- readRDS("./data/points_projection.rds")
pts  <- readRDS("./data/players_points.rds")

dtf <- pts %>% 
  #rename(nfl_id=id.y) %>% 
  inner_join(proj, by=c("id"="id", "week", "season"))  %>% 
  select(id, nfl_id, season, week, pos, data_src, pts.proj, points )  %>% 
  #filter(week<cur.week) %>% 
  mutate(error=points-pts.proj)

dtf %>% 
  ggplot() +
  geom_point(aes(x=pts.proj, y=points, color=pos)) +
  facet_grid(pos~data_src, scales = "free") +
  geom_abline(intercept=0, slope=1, linetype="dashed") + 
  ggtitle(glue("Weeks 1~3 - Projection"), subtitle = "By Position and Source Site") +
  xlim(0,30) + ylim(0,30)

ggsave("./static/img/week4_players_proj_points.png", device = "png", 
       units = "in", width = 18.5, height = 10.7, dpi = 70 )

dtf %>% 
  ggplot() +
  geom_density(aes(x=error, fill=pos)) +
  facet_grid(pos~data_src, scales = "free") +
  geom_vline(xintercept = 0, color="black", linetype="dashed") +
  ggtitle("Week 1~3 - Error Distribution", subtitle = "By Position and Source Site") +
  theme_minimal()

ggsave("./static/img/week4_projection_errors.png", device = "png", 
       units = "in", width = 18.5, height = 10.7, dpi = 70 )

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



