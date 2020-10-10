library(tidyverse)
library(glue)
library(ggrepel)

.week <- 5
sim <- readRDS(glue("./data/simulation_v5_week{.week}_preWaiver.rds"))


pHist <- sim$players_stats %>% 
  unnest(weekPts) %>% 
  select(playerId, name, position, byeWeek, week, weekPts) %>% 
  filter(byeWeek!=week, week<.week) %>% 
  mutate( weekPts = if_else(is.na(weekPts), 0, weekPts) ) %>% 
  group_by(playerId, name, position) %>% 
  summarise(
    avgPts = mean(weekPts),
    sdPts  = sd(weekPts)
  ) %>% 
  arrange(desc(avgPts))

pProj<- sim$proj_table %>% 
  inner_join(select(sim$players_id, id, playerId=nfl_id), by="id") %>% 
  select(17,10:16)


pHist %>% 
  inner_join(pProj) %>% 
  filter(position=="WR") %>% 
  ungroup() %>% 
  top_n(n = 50, wt=avgPts) %>% 
  ggplot(aes(points, avgPts)) +
  geom_abline(slope=1) +
  geom_point() +
  geom_text_repel(aes(label=name)) +
  labs(x="projecao",y="pontuacao") +
  theme_minimal()

