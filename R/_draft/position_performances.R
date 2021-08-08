library(tidyverse)
library(glue)
library(ggimage)


draft_rosters <- readRDS("./data/drafted_teams_rosters.rds")

histRosters <- 1:16 %>% 
  map_df(function(.week){
    sim <- readRDS(glue("./data/simulation_v5_week{.week}_final.rds"))
    bind_rows(
      select(sim$matchup_sim, week, team.id=awayTeam.teamId, points=awayTeam.totalPts),
      select(sim$matchup_sim, week, team.id=homeTeam.teamId, points=homeTeam.totalPts)
    ) %>% 
      inner_join(select(sim$teams, team.id=teamId, team.name=name, rosters), by="team.id") %>% 
      return()
  })

histRosters <- histRosters %>% 
  group_by(team.id) %>% 
  arrange(week) %>% 
  mutate(acc.points = cumsum(points)) %>% 
  ungroup() %>% 
  arrange(team.id, week)

sim <- readRDS(glue("./data/simulation_v5_week16_final.rds"))

histPoints <- sim$players_stats %>% 
  select(playerId, position, weekPts) %>% 
  unnest(weekPts)

rosterPerf <- histRosters %>% 
  unnest(rosters) %>% 
  filter(rosterSlotId<20) %>% 
  select(week, team.id, team.name, points, acc.points, playerId) %>% 
  inner_join(histPoints, by=c("playerId","week"))

sim <- readRDS(glue("./data/simulation_v5_week1_final.rds"))
  
rosterPerf %>% 
  filter(week <=13 ) %>% 
  group_by(team.id, position) %>% 
  arrange(week) %>% 
  summarise( position.points = sum(weekPts, na.rm=T), .groups="drop") %>% 
  group_by(team.id) %>% 
  mutate( team.points = sum(position.points) ) %>% 
  ungroup() %>% 
  inner_join( select(sim$teams, team.id=teamId, name, imageUrl), by="team.id" ) %>% 
  mutate( nickname=str_extract(name, "\\b(\\w+)$") ) %>% 
  mutate( nickname=fct_reorder(nickname, team.points )) %>% 
  mutate( position=fct_reorder(position, position.points )) %>% 
  ggplot(aes(x=nickname, y=position.points, fill=position)) +
  geom_bar(position="stack", stat="identity", width = .5) +
  geom_image(aes(y=team.points, image=imageUrl), nudge_y=60) +
  labs(x="", y="points", title="Season points",
       subtitle="week 1-13") +
  scale_fill_brewer(type="qual", palette = "Set1") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle=-60, hjust = 0) )

  
rosterPerf %>% 
  filter(week <=13 ) %>% 
  group_by(team.id, position) %>% 
  arrange(week) %>% 
  summarise( position.points = sum(weekPts, na.rm=T), .groups="drop") %>% 
  group_by(team.id) %>% 
  mutate( team.points = sum(position.points) ) %>% 
  ungroup() %>% 
  inner_join( select(sim$teams, team.id=teamId, name, imageUrl), by="team.id" ) %>% 
  mutate( nickname=str_extract(name, "\\b(\\w+)$") ) %>% 
  mutate( position.points = position.points/team.points ) %>% 
  mutate( nickname=fct_reorder(nickname, team.points )) %>% 
  mutate( position=fct_reorder(position, position.points )) %>% 
  ggplot(aes(x=nickname, y=position.points, fill=position)) +
  geom_bar(position="stack", stat="identity", width = 1) +
  geom_image(aes(y=0, image=imageUrl), nudge_y=-0.01) +
  labs(x="", y="points", title="Season points",
       subtitle="week 1-13") +
  scale_fill_brewer(type="qual", palette = "Set1") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle=-60, hjust = 0) )
 


pivot_positions <- rosterPerf %>% 
  filter(week <=13 ) %>% 
  group_by(team.id, position) %>% 
  arrange(week) %>% 
  summarise( position.points = sum(weekPts, na.rm=T), .groups="drop") %>% 
  group_by(team.id) %>% 
  mutate( team.points = sum(position.points) ) %>% 
  ungroup() %>% 
  inner_join( select(sim$teams, team.id=teamId, name, imageUrl), by="team.id" ) %>% 
  mutate( nickname=str_extract(name, "\\b(\\w+)$") ) %>% 
  mutate( nickname=fct_reorder(nickname, team.points )) %>% 
  mutate( position=fct_reorder(position, position.points )) %>% 
  select(nickname, imageUrl, position, position.points)%>% 
  pivot_wider(id_cols = c(nickname, imageUrl), names_from="position", values_from="position.points")

pivot_positions %>% 
  ggplot(aes(x=RB, y=WR)) +
  geom_image(aes(image=imageUrl))+
  geom_vline(aes(xintercept=mean(RB)), color="grey", linetype="dashed") +
  geom_hline(aes(yintercept=mean(WR)), color="grey", linetype="dashed") +
  labs(title="Distribuição de Pontos por Posição",
       subtitle="Semanas 1-13 | WRs e RBs") +
  theme_classic()

pivot_positions %>% 
  ggplot(aes(x=TE, y=QB)) +
  geom_image(aes(image=imageUrl))+
  geom_vline(aes(xintercept=mean(TE)), color="grey", linetype="dashed") +
  geom_hline(aes(yintercept=mean(QB)), color="grey", linetype="dashed") +
  labs(title="Distribuição de Pontos por Posição",
       subtitle="Semanas 1-13 | QBs e TEs") +
  theme_classic()

pivot_positions %>% 
  ggplot(aes(x=K, y=DEF)) +
  geom_image(aes(image=imageUrl))+
  geom_vline(aes(xintercept=mean(K)), color="grey", linetype="dashed") +
  geom_hline(aes(yintercept=mean(DEF)), color="grey", linetype="dashed") +
  labs(title="Distribuição de Pontos por Posição",
       subtitle="Semanas 1-13 | Defesa e kickers") +
  theme_classic()
