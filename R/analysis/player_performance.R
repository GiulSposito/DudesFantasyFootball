library(tidyverse)
library(plotly)

# carrega players stats e escalacao da ultima semana
players <- readRDS("./data/players_points.rds")
matchups <- readRDS("./data/week8_simulation_v2.rds")

# decompoe a escalacao em player | team
c("home","away") %>% 
  map(function(.prefix, .matchups){
    .matchups %>%
      select( starts_with(.prefix) ) %>%
      set_names(gsub(paste0(.prefix, "."), "", names(.))) %>% 
      select( teamId, name, roster ) %>% 
      mutate(
        roster = map(roster, function(.team){
          .team %>% 
            select(id, player=name, position, rosterSlot)
        })
      ) %>% 
      unnest(roster)
  },
  .matchups = matchups) %>% 
  bind_rows() %>% 
  select(id, team=name) %>% 
  filter( !is.na(id) ) -> teams

# limpa tabela de jogadores
players %>% 
  filter(points!=0) %>% 
  select(id, firstName, lastName, position, week, points) %>% 
  inner_join(teams, by="id") %>% 
  mutate(
    name = paste0(firstName, " ", lastName)
  ) %>% 
  mutate(
    team = case_when(
      is.na(team) ~ "*FA",
      TRUE ~ team
    ),
    position = as.factor(position)
  ) %>% 
  mutate(team=as.factor(team)) -> team.players

# isola chaves dos jogadores
team.players %>% 
  select(id, name, position, team) %>% 
  distinct() -> players.key

# faz a estatistica de media e desvio padrao para janelas de 3, 4, 5 e 8 semanas
c(3,4,5,8) %>% 
  map(
    function(.nweeks, .team, .last.week){
      team.players %>% 
        filter(week > (.last.week - .nweeks) ) %>% 
        select(id, week, points) %>% 
        group_by(id) %>%   
        summarise(
          pts.mean = mean(points, na.rm = T),
          pts.sd   = sd(points, na.rm = T),
          sharpe   = pts.mean / pts.sd,
          weeks.rng = n()
        ) %>% 
        return()
    },
    .team = team.players, 
    .last.week = max(team.players$week, na.rm = T)
  ) %>% 
  bind_rows() %>% 
  distinct() %>% # elimina casos (excepcionais) em que a janela pegou o mesmo numero de semanas
  filter( !is.na(pts.sd) & !is.na(pts.mean) ) %>% 
  mutate( weeks.rng = as.factor(weeks.rng)) %>% 
  inner_join(players.key, by="id") -> players.stats

unique(players.stats$position) %>% 
  map(function(.pos, .players){
    .players %>% 
      filter(position==.pos) %>% 
      select(pts.mean, pts.sd, weeks.rng, sharpe) %>% 
      mutate_all(as.numeric) %>% 
      kmeans(10) -> km 
    
    .players %>% 
      filter(position==.pos) %>% 
      mutate(cluster = km$cluster) %>% 
      select(id, cluster) %>% 
      return()
  }, 
  .players=players.stats) %>% 
  bind_rows() %>% 
  inner_join(players.stats, by="id")

players.stats %>% 
  select(pts.mean, pts.sd, weeks.rng, sharpe) %>% 
  mutate_all(as.numeric) %>% 
  kmeans(10) -> km

km$cluster

c(3,4,6,8) %>% 
  map(function(.nweeks, .teams){
    .teams %>% 
      filter( )
      
      
      group_by(id, name, team, position) %>% 
        top_n(.weeks,week) %>%
        summarise(
          pts.mean = mean(points, na.rm = T),
          pts.sd   = sd(points, na.rm=T ),
          week.rng = .weeks,
          sharpe   = pts.mean/pts.sd
        ) %>%
        return()
  },
  .teams = team.players) %>% 
  bind_rows() %>% 
  filter( sharpe!=Inf ) -> players.sharp


players.sharp %>% 
  filter(position=="TE") %>% 
  arrange(desc(pts.mean)) %>% View()

players.sharp %>% 
  filter(id==12391) %>% View()

team.players %>% 
  filter( id==12391 ) %>% 
  select(id, firstName, lastName, position, team, week, points)


  plot_ly(x=~pts.sd, y=~pts.mean, color=~team,
          type="scatter", mode="markers", 
          text=~paste("Name: ", name)) 


  saveRDS(team.players, "./data/team_players_stats.rds")