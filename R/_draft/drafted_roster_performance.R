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
  select(playerId, weekPts) %>% 
  unnest(weekPts)

drafted <- draft_rosters %>% 
  unnest(rosters) %>% 
  select(teamId, team=name, playerId) %>% 
  mutate( drafted=T )

draftedRosters <- histRosters %>% 
  unnest(rosters) %>% 
  filter(rosterSlotId<20) %>% 
  select(week, team.id, team.name, points, acc.points, playerId) %>% 
  left_join(drafted, by=c("team.id"="teamId", "playerId")) %>% 
  mutate( drafted=if_else(is.na(drafted),F,drafted)) %>% 
  select(-team) %>% 
  inner_join(histPoints, by=c("playerId","week"))

# % de pontos de jogador draftado
draftedRosters %>% 
  group_by(week, team.id, team.name, points, drafted) %>% 
  summarise(weekPts = sum(weekPts, na.rm = T), .groups="drop") %>%
  mutate(week.pct=weekPts/points) %>% 
  ggplot(aes(x=week, y=week.pct, fill=drafted)) +
  geom_bar(position="stack", stat="identity", width=1) +
  geom_vline(xintercept = 13.5, linetype="dashed", color="black") +
  facet_wrap(team.name ~ ., ncol = 2 ) +
  theme_minimal()

# pontos de jogador draftado
draftedRosters %>% 
  group_by(week, team.id, team.name, points, drafted) %>% 
  summarise(weekPts = sum(weekPts, na.rm = T), .groups="drop") %>%
  mutate(week.pct=weekPts/points) %>% 
  ggplot(aes(x=week, y=weekPts, fill=drafted)) +
  geom_bar(position="stack", stat="identity", width=1) +
  geom_vline(xintercept = 13.5, linetype="dashed", color="black") +
  facet_wrap(team.name ~ ., ncol = 2 ) +
  theme_minimal()

# pontos de jogador draftado acumulado
draftedRosters %>% 
  group_by(week, team.id, team.name, points, drafted) %>% 
  summarise(weekPts = sum(weekPts, na.rm = T), .groups="drop") %>%
  group_by(team.id, team.name) %>% 
  mutate(acc.points = cumsum(points) ) %>% 
  group_by(team.id, team.name, drafted ) %>% 
  mutate(acc.weekPts = cumsum(weekPts) ) %>% 
  ungroup() %>% 
  mutate( acc.week.pct = acc.weekPts/acc.points ) %>% 
  ggplot(aes(x=week, y=acc.weekPts, fill=drafted)) +
  geom_area() +
  #geom_bar(position="stack", stat="identity", width=1) +
  geom_vline(xintercept = 13.5, linetype="dashed", color="black") +
  facet_wrap(team.name ~ ., ncol = 2 ) +
  theme_minimal()

# % pontos de jogador draftado acumulado
draftedRosters %>% 
  group_by(week, team.id, team.name, points, acc.points, drafted) %>%
  arrange( team.id, week ) %>% 
  summarise(weekPts = sum(weekPts, na.rm = T), .groups="drop") %>%
  mutate(acc.weekPts = cumsum(weekPts) ) %>% 
  ungroup() %>% 
  mutate( acc.week.pct = acc.weekPts/acc.points ) %>%
  arrange(team.id, week) %>% View()
  ggplot(aes(x=week, y=acc.week.pct, fill=drafted)) +
  # geom_area() +
  geom_bar(position="stack", stat="identity", width=1) +
  geom_vline(xintercept = 13.5, linetype="dashed", color="black") +
  facet_wrap(team.name ~ ., ncol = 2 ) +
  theme_minimal()

sim <- readRDS(glue("./data/simulation_v5_week1_final.rds"))
  
  
draftedRosters %>% 
  group_by(week, team.id, points, acc.points, drafted) %>% 
  summarise(week.pts=sum(weekPts, na.rm=T), .groups="drop") %>%
  group_by(team.id, drafted) %>% 
  mutate( acc.week.pts = cumsum(week.pts)) %>% 
  ungroup() %>% 
  arrange(team.id, week) %>% 
  filter(week==13) %>% 
  pivot_wider(id_cols=c(team.id, acc.points), names_from=drafted, values_from=acc.week.pts) %>% 
  set_names(c("team.id","acc.points","not.drafted","drafted")) %>% 
  mutate( not.drafted = if_else(is.na(not.drafted), acc.points-drafted, not.drafted)) %>% 
  inner_join( select(sim$teams, team.id=teamId, name, imageUrl), by="team.id" ) %>% 
  mutate( nickname=str_extract(name, "\\b(\\w+)$") ) %>% 
  ggplot(aes(x=not.drafted, y=drafted), size=30) +
  geom_image(aes(image=imageUrl)) +
  geom_text(aes(label=nickname), nudge_y = -20, size=3) +
  labs(title="Contribuição na pontuação, dos titulares, entre draftados e não draftados", 
       subtitle="Week 1-13") +
  theme_minimal() 


draftedRosters %>% 
  group_by(week, team.id, points, acc.points, drafted) %>% 
  summarise(week.pts=sum(weekPts, na.rm=T), .groups="drop") %>%
  group_by(team.id, drafted) %>% 
  mutate( acc.week.pts = cumsum(week.pts)) %>% 
  ungroup() %>% 
  arrange(team.id, week) %>% 
  filter(week==13) %>% 
  pivot_wider(id_cols=c(team.id, acc.points), names_from=drafted, values_from=acc.week.pts) %>%
  set_names(c("team.id","acc.points","not.drafted","drafted")) %>%
  mutate( not.drafted = if_else(is.na(not.drafted), acc.points-drafted, not.drafted)) %>%
  pivot_longer(cols = c(not.drafted, drafted), names_to="drafted", values_to="acc.week.pts" ) %>% 
  inner_join( select(sim$teams, team.id=teamId, name, imageUrl), by="team.id" ) %>% 
  mutate( drafted = drafted=="drafted" ) %>% 
  mutate( nickname=str_extract(name, "\\b(\\w+)$") ) %>% 
  mutate( nickname=fct_reorder(nickname, acc.points)) %>% 
  ggplot(aes(x=nickname, y=acc.week.pts, fill=drafted)) +
  geom_bar(position="stack", stat="identity", width = .5) +
  geom_image(aes(y=acc.points, image=imageUrl), nudge_y=60) +
  labs(x="", y="points", title="Season points",
       subtitle="week 1-13") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle=-60, hjust = 0) )
  