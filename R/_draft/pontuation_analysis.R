library(tidyverse)

points <- readRDS("./data/players_points.rds")
projections <- readRDS("./data/players_projections.rds") %>% 
  rename(pts.proj = points)

points %>% 
  inner_join(projections, by = c("src_id", "season", "week")) %>%
  select(id, player, position, season, week, points, data_src, pts.proj) %>% 
  mutate( data_src = as.factor(data_src),
          position = as.factor(position) ) -> player.data


player.data %>% 
  ggplot(aes(x=points, y=pts.proj)) +
  geom_point(aes(color=position)) +
  geom_abline(intercept = 0, slope=1, linetype=2) +
  facet_grid( position~data_src ) +
  theme_bw() +
  theme( legend.position = "none" )


player.data %>% 
  mutate( error = (pts.proj-points) ) %>% 
  ggplot() +
  geom_histogram(aes(error, fill=position)) +
  facet_grid( position~data_src ) +
  theme_bw() +
  theme( legend.position = "none" )
  
  
