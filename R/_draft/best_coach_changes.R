library(tidyverse)
library(glue)

w1 = 2
w2 = 3

sim1 <- readRDS(glue("./data/simulation_v5_week{w1}_posMNF.rds"))
sim2 <- readRDS(glue("./data/simulation_v5_week{w2}_posMNF.rds"))

team1 <- sim1$teams %>% 
  unnest(rosters) %>% 
  select(teamId, playerId)

team2 <- sim2$teams %>% 
  unnest(rosters) %>% 
  select(teamId, playerId)

mantidos <- team1 %>% 
  inner_join(team2, by = c("teamId", "playerId"))

liberados <- team1 %>% 
  anti_join(mantidos) %>% 
  mutate( status = "liberado" )

contratados <- team2 %>% 
  anti_join(mantidos) %>% 
  mutate( status = "contratado" )

changes <- bind_rows(liberados, contratados)

sim2$players_stats %>% 
  unnest(weekPts) %>% 
  filter(week==w2) %>% 
  select(playerId, name, weekPts) %>% 
  inner_join(changes) %>% 
  mutate( weekPts = if_else(status=="contratado", weekPts, -weekPts)) %>% 
  group_by(teamId) %>% 
  summarise( saldo = sum(weekPts, na.rm = T)) %>% 
  inner_join(select(sim1$teams, teamId, name)) %>% 
  select(teamId, name, saldo) %>% 
  arrange(desc(saldo))
  
sim2$players_stats %>% 
  unnest(weekPts) %>% 
  filter(week==w2) %>% 
  select(playerId, name, weekPts) %>% 
  inner_join(changes) %>% 
  mutate( weekPts = if_else(status=="contratado", weekPts, -weekPts)) %>% 
  filter(teamId==2) %>% 
  arrange(weekPts)
  