library(tidyverse)

sim <- readRDS("./data/simulation_v5_week3_posSNF.rds")

sim$ptsproj %>% 
  filter(week==3, pos=="RB") %>% 
  mutate(source_type=case_when(
    (data_src == "floor" | data_src == "ceiling") ~ "floor_ceiling",
    str_detect(data_src, "ERROR") ~ "site_error",
    T ~ "sites"
  )) %>% 
  ggplot(aes(x=pts.proj, fill=source_type)) +
  geom_density(alpha=.5) +
  facet_wrap(.~pos, scales = "free") +
  labs(title="Projection", subtitle = "Per Data Sources and Positions") +
  theme_minimal()

sim$ptsproj %>% 
  filter(week==3, pos=="WR") %>% 
  mutate(source_type=case_when(
    (data_src == "floor" | data_src == "ceiling") ~ "floor_ceiling",
    str_detect(data_src, "ERROR") ~ "site_error",
    T ~ "sites"
  )) %>%
  filter(source_type=="sites") %>% 
  ggplot(aes(x=pts.proj, fill=data_src)) +
  geom_density(alpha=.5) +
  facet_wrap(.~pos, scales = "free") +
  labs(title="Projection", subtitle = "Per Data Sources and Positions") +
  theme_minimal()


sim$players_stats %>% 
  filter(position=="WR") %>% 
  unnest(weekPts) %>% 
  select(playerId, position, week, weekPts) %>% 
  arrange(playerId) %>% 
  ggplot(aes(x=weekPts, fill=position)) +
  geom_density(alpha=.5) +
  facet_wrap(.~position, scales = "free") +
  labs(title="Real Points") +
  theme_minimal()

sim$ptsproj %>% 
  filter(pos=="WR") %>% 
  ggplot(aes(x=pts.proj, fill=pos)) +
  geom_density(alpha=.5) +
  facet_wrap(.~pos) +
  labs(title="Projection Points Simulation", scales = "free") +
  xlim(0,40)+
  theme_minimal()


sim$ptsproj %>% summary()

sim$players_stats %>% 
  select(playerId, weekPts) %>% 
  unnest(weekPts) %>% 
  inner_join(select(sim$players_id,id, playerId=nfl_id), by = "playerId") %>% 
  inner_join(sim$ptsproj, by = c("week", "id"))


