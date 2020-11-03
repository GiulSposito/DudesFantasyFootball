library(tidyverse)

sim <- readRDS("./data/simulation_v5_week6_posWaivers.rds")

player_perf <- sim$players_stats %>% 
  select(playerId, position, weekPts) %>% 
  unnest(weekPts) %>% 
  filter(complete.cases(.)) %>% 
  # add_count(playerId,name = "games") %>% 
  group_by(playerId, position) %>% 
  summarise(
    # pts.avg = mean(weekPts),
    # pts.sd  = sd(weekPts), 
    games   = n(),
    stats   = list(as.tibble(as.list(Rmisc::CI(weekPts))))
  ) %>% 
  unnest(stats)

player_proj <- sim$proj_table %>% 
  inner_join(select( sim$players_id , id, playerId=nfl_id ), by="id") %>% 
  select(playerId, floor, points, ceiling )

g <- player_proj %>% 
  inner_join(player_perf, by="playerId") %>% 
  ggplot(aes(x=mean, y=points, color=position)) +
  geom_point() +
  geom_errorbar(aes(ymin=floor, ymax=ceiling)) +
  #geom_errorbarh(aes(xmin=lower, xmax=upper)) +
  geom_abline(slope=1, color="grey", linetype="dashed") +
  xlim(0, 35) +
  theme_minimal()

plotly::ggplotly(g)

