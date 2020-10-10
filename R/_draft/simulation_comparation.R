sim <- readRDS("./data/simulation_v5_week4_posMNF.rds")


sim$players_stats %>% 
  inner_join(select(sim$players_sim, playerId, simulation.org)) %>% 
  filter(position=="QB") %>% 
  unnest(weekPts) %>% 
  filter(week==4) %>% 
  unnest(simulation.org) %>% 
  ggplot() +
  geom_density(aes(weekPts), fill="grey", alpha=.8) +
  geom_density(aes(simulation.org), fill="red", alpha=.3) +
  lims(x=c(0,40)) +
  theme_minimal()

sim$players_sim %>% 
  inner_join(sim$players_stats, by="playerId") %>% 
  filter(position=="WR") %>% 
  select(playerId, simulation.org) %>% 
  unnest(simulation.org) %>% 
  ggplot(aes(x=simulation.org)) +
  geom_histogram(fill="red") +
  lims(x=c(0,40)) +
  theme_minimal()


sim$players_stats %>% 
  inner_join(select(sim$players_sim, playerId, simulation.org)) %>% 
  filter(position=="RB") %>% 
  unnest(weekPts) %>% 
  filter(week==4) %>% 
  unnest(simulation.org) %>% 
  group_by(playerId) %>% 
  mutate( predPts = median(simulation.org, na.rm=T) ) %>% 
  ungroup() %>% 
  mutate(weekPts==if_else(is.na(weekPts), 0, weekPts)) %>% 
  ggplot() +
  geom_density(aes(simulation.org), fill="red", alpha=.3) +
  geom_vline(aes(xintercept=weekPts), linetype="dashed") +
  geom_vline(aes(xintercept=predPts), linetype="dotted") +
  facet_wrap(name~., scales = "free")+
  lims(x=c(0,50)) +
  theme_minimal()
