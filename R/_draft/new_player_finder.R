library(tidyverse)
library(glue)
library(ggrepel)

.week <- 5
sim <- readRDS(glue("./data/simulation_v5_week{.week}_preWaiver.rds"))


pHist <- sim$players_stats %>% 
  unnest(weekPts) %>% 
  select(playerId, name, position, byeWeek, week, weekPts) %>% 
  filter(byeWeek!=week, week<.week) %>% 
  # mutate( weekPts = if_else(is.na(weekPts), 0, weekPts) ) %>%
  filter(complete.cases(.)) %>% 
  group_by(playerId, name, position) %>% 
  summarise(
    avgPts = mean(weekPts),
    sdPts  = sd(weekPts),
    games  = n()
  ) %>% 
  ungroup() %>% 
  arrange(desc(avgPts))

pProj<- sim$proj_table %>% 
  inner_join(select(sim$players_id, id, playerId=nfl_id), by="id") %>% 
  select(17,10:16)

rosters <- sim$teams %>% 
  unnest(rosters) %>% 
  select(teamId, team=name, playerId)

pHist %>% 
  left_join(rosters, by="playerId") %>% 
  left_join(pProj, by="playerId") %>% 
  mutate(team=if_else(is.na(team),"FA", team)) %>% 
  filter(is.na(teamId)|teamId==4) %>% 
  filter(position=="WR") %>% 
  top_n(n = 50, wt=avgPts) %>% 
  ggplot(aes(avgPts, sdPts)) +
  geom_abline(slope=1) +
  geom_point(aes(color=team, size=games)) +
  geom_text_repel(aes(label=name)) +
  labs(x="sd",y="mean") +
  theme_minimal()
 
