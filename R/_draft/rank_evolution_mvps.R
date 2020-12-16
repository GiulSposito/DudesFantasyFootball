library(tidyverse)
library(ggimage)

sim <- readRDS("./data/simulation_v5_week13_final.rds")

players <- sim$players_stats



mvps <- players %>% 
  select(playerId, position, weekPts) %>% 
  unnest(weekPts) %>% 
  filter(week==13) %>% 
  group_by(position) %>% 
  slice_max(weekSeasonPts, n=15) %>% 
  ungroup() %>% 
  select(playerId)


players %>% 
  inner_join(mvps, by="playerId") %>% 
  select(playerId, name, firstName, lastName, position, smallImageUrl, weekPts) %>% 
  unnest(weekPts) %>% 
  group_by(week, position) %>% 
  mutate( rank = rank(-weekSeasonPts, playerId, ties.method = "first") ) %>% 
  ungroup() %>% 
  arrange(week, position, rank) %>% 
  filter( position == "K" ) %>% 
  ggplot(aes(x=week, y=reorder(rank,-rank), group=firstName)) +
  geom_line(aes(color=name), size=2, alpha=.6) +
  geom_image(aes(image=smallImageUrl), size=.04, by="width") +
  geom_text(aes(label=paste0(lastName, " (", weekSeasonPts,")")), size=2.5, color="black", nudge_y = -.33) +
  ylab("rank") +
  theme_void()


