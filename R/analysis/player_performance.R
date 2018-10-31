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
  left_join(teams, by="id") %>% 
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
  filter( pts.sd != 0 ) %>% 
  inner_join(players.key, by="id") -> players.stats

# clustering the positions
unique(players.stats$position) %>% 
  map(function(.pos, .players){
    .players %>% 
      filter(position==.pos) %>% 
      select(pts.mean, pts.sd, sharpe) %>% 
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
  inner_join(players.stats, by="id") -> players.performance

.position = "WR"
.weeks = 8

players.performance %>% 
  filter(position==.position, weeks.rng<=.weeks) %>%
  group_by(id) %>% 
  filter(weeks.rng==max(weeks.rng)) %>% 
  ungroup() %>% 
  mutate( status = case_when(
    team == "*FA" ~ "Free",
    team == "Amparo Bikers" ~ "Bikers",
    TRUE ~ "Unavaiable"
  )) %>%
  plot_ly(x=~pts.sd, y=~pts.mean, 
          color=~status, colors="Set1",
          symbol = ~cluster,
          text=~paste("id: ", id,
                      "\nname: ", name,
                      "\nsharpe: ", round(sharpe,2),
                      "\nteam: ", team,
                      "\nweeks: ", weeks.rng))


saveRDS(players.performance, "./data/team_players_performance.rds")
