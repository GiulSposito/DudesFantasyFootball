library(tidyverse)

sim <- readRDS("./data/simulation_v5_week6_posWaivers.rds")

sel_ids <- sim$proj_table$id %>% 
  sample(100)

my <- sim$ptsproj %>% 
  filter(id %in% sel_ids, week==6) %>%
  arrange(id) %>% 
  group_by(id) %>% 
  summarise( stat = list(as_tibble(as.list(Rmisc::CI(pts.proj, .9))))) %>% 
  unnest(stat)

ffa <- sim$proj_table %>% 
  filter( id %in% sel_ids ) %>% 
  select(id, ceiling, points, floor)

inner_join(my,ffa) %>% 
  ggplot() +
  geom_errorbar(aes(x=mean, y=points, ymin=floor, ymax=ceiling)) +
  geom_errorbarh(aes(x=mean, y=points, xmin=lower, xmax=upper)) +
  geom_abline(slope = 1, color="grey", linetype="dashed") +
  labs(x="my", y="ffa") +
  ggtitle("Comparativo FFA+Errors vs FFA onlye") +
  theme_minimal()

## previsão contra realizado 

sim <- readRDS("./data/simulation_v5_week5_final.rds")

actual <- sim$players_stats %>% 
  unnest(weekPts) %>% 
  filter(week==5) %>% 
  select(playerId, position, weekPts)

proj <- sim$proj_table  %>% 
  inner_join(select(sim$players_id, id, playerId="nfl_id"), by="id") %>% 
  select(playerId, floor, points, ceiling)

proj2 <- sim$ptsproj %>% 
  filter(id %in% sel_ids, week==5) %>%
  inner_join(select(sim$players_id, id, playerId="nfl_id"), by="id") %>% 
  group_by(playerId) %>% 
  summarise( stat = list(as_tibble(as.list(Rmisc::CI(pts.proj, .9))))) %>% 
  unnest(stat)


proj %>% 
  inner_join(actual, by="playerId") %>% 
  sample_n(200) %>% 
  ggplot(aes(x=weekPts, y=points, color=position)) +
  geom_point() +
  geom_errorbar(aes(ymin=floor, ymax=ceiling)) +
  geom_abline(slope=1, color="grey", linetype="dashed") +
  theme_minimal()

proj2 %>% 
  inner_join(actual, by="playerId") %>% 
  ggplot(aes(x=weekPts, y=mean, color=position)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  geom_abline(slope=1, color="grey", linetype="dashed") +
  theme_minimal()
