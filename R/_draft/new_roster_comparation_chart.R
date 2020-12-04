library(tidyverse)
library(glue)

sim <- readRDS("./data/simulation_v5_week11_posWaivers.rds")

sim$teams %>% 
  select(teamId, name, imageUrl, week.stats) %>% 
  unnest(week.stats) %>% 
  filter(week==.week) %>% 
  inner_join(sim$teams_sim, by="teamId") %>% 
  select(-simulation) %>% 
  rename(sim=simulation.org) %>%
  unnest(sim) %>% 
  mutate(name=fct_reorder(name, sim),
         pts=if_else(pts==0, as.numeric(NA), pts)) %>% 
  ggplot(aes(x=sim, y=name, fill=name)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.1,0.5,0.9), scale=1, alpha=.6) +
  geom_point(aes(x=pts, y=name), color="black", fill="red", shape=24) +
  theme_ridges() +
  theme( legend.position = "none" ) +
  scale_y_discrete(expand=c(0.,0)) +
  scale_x_continuous(expand = c(0.01,0)) +
  labs(x="points",y="")

sim$players_sim %>% 
  select(teamId, teamName, playerId, rosterSlotId, pts.proj, weekPts, simulation.org ) %>% 
  inner_join(select(sim$players_stats,playerId, name, position), by="playerId") %>% 
  relocate(teamId, teamName, playerId, name, position, everything()) %>% 
  filter(teamId%in%c(1,2)) %>% 
  mutate(name=as.factor(glue("{name} [{position}]"))) %>% 
  arrange(rosterSlotId) %>% 
  unnest(simulation.org) %>% 
  mutate(name=fct_reorder(name, -rosterSlotId)) %>% 
  ggplot(aes(x=simulation.org, y=name, fill=position)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = c(0.1,0.5,0.9), scale=1, alpha=.6) +
  geom_point(aes(x=weekPts, y=name), color="black", fill="red", shape=24) +
  theme_ridges() +
  theme( legend.position = "none" ) +
  scale_y_discrete(expand=c(0.,0)) +
  scale_x_continuous(expand = c(0.01,0)) +
  facet_wrap(rosterSlotId~., ncol = 1, scales = "free") +
  labs(x="points",y="")
 

sim$players_sim %>% 
  select(teamId, teamName, playerId, rosterSlotId, pts.proj, weekPts, simulation.org ) %>% 
  inner_join(select(sim$players_stats,playerId, name, position), by="playerId") %>% 
  relocate(teamId, teamName, playerId, name, position, everything()) %>% 
  filter(teamId%in%c(1,2)) %>% 
  mutate(name=as.factor(glue("{name} [{position}]"))) %>% 
  arrange(rosterSlotId) %>% 
  unnest(simulation.org) %>% 
  mutate(name=fct_reorder(name, -rosterSlotId)) %>% 
  ggplot(aes(x=simulation.org, y=name, fill=teamName)) +
  geom_boxplot() +
  # geom_density_ridges(quantile_lines = TRUE, quantiles = c(0.1,0.5,0.9), scale=1, alpha=.6) +
  geom_point(aes(x=weekPts, y=name), color="black", fill="red", shape=24) +
  # theme_ridges() +
  #theme( legend.position = "none" ) +
  # scale_y_discrete(expand=c(0.,0)) +
  # scale_x_continuous(expand = c(0.01,0)) +
  facet_wrap(rosterSlotId~., ncol = 1, scales = "free") +
  labs(x="points",y="") +
  theme_minimal()



