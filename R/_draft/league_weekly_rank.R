library(tidyverse)
library(lubridate)
library(glue)
library(ffanalytics)
library(ggimage)

# parametros de execucao
weeks <- 1:4

# check Fantasy API
source("./R/import/checkFantasyAPI.R")
if(!checkFantasyAPI(1)) stop("Unable to access Fantasy API!")

# import machups
source("./R/import/import_matchups.R")
matchups <- weeks %>% 
  map(
    importMatchups,
    .saveToFile = F
  )

# para cada time do roster tira informacoes de ponto, se ganhou e qual o rank
extractTeam <- . %>% 
  select(id, team=name, logoUrl, pts, outcome) %>% 
  mutate(
    id = as.integer(id),
    win=(outcome=="win"),
    loss=(outcome!="win"),
    pts=as.numeric(pts)
  ) %>% 
  as_tibble()

# adiciona informacoes do oponente para fazer o ranking H2H
addOpponent <- function(.t, .o){ 
  .t %>% 
    mutate(
      pts.ctr = .o$pts,
      op.id   = .o$id,
      op.team = .o$team,
      op.out  = .o$outcome
    ) %>% 
    return()
}

# para cada jogo dentro da semana
# processa os times
extractGame <- function(.game) {
  
  .teams <- .game[[1]]$matchup 
  
  at <- extractTeam(.teams$awayTeam)
  ht <- extractTeam(.teams$homeTeam)
  
  at <- addOpponent(at, ht)
  ht <- addOpponent(ht, at)
  
  bind_rows(at,ht) %>% 
    return()

}

# para cada conjunto de jogos dentre de uma semana
# processa o jogo
extractWeekGames <- function(.weekgames){
  .weekgames %>% 
    map_df(extractGame)
}

# para todas as semanas
wrank <- matchups %>% 
  map_df(
    extractWeekGames,
    .id="week"
  ) # %>% 
  # mutate( id=factor(id), team=factor(team) )

# cria a tabela (long) de confrontos diretos
h2h.table <- wrank %>% 
  select(week, id, team, outcome, op.out, op.team, op.id) %>% 
  mutate( h2h.balance = (outcome=="win") - (op.out=="win") ) %>% 
  group_by(id, team, op.id, op.team) %>% 
  summarise( balance = sum(h2h.balance)) %>% 
  ungroup()

# salva dados
# saveRDS(wrank, "./data/weekly_results.rds")

wrank <- wrank %>% 
  group_by(team) %>% 
  arrange(team,week) %>% 
  mutate( 
    wins = cumsum(win),
    losses = cumsum(loss),
    pts.pro = cumsum(pts),
    pts.ag  = cumsum(pts.ctr)
  ) %>% 
  ungroup()

wrank <- wrank %>% 
  group_by(week, wins, losses) %>% 
  nest() %>% 
  mutate(
    data = map(data, function(.x, .h2ht){
      expand.grid(id=.x$id, op.id=.x$id) %>% 
        inner_join(.h2ht, by = c("id", "op.id")) %>% 
        group_by(id, team) %>% 
        summarise(h2h.balance=sum(balance)) %>% 
        ungroup() %>% 
        right_join(.x, by = c("id", "team")) %>% 
        mutate( h2h.balance = ifelse(is.na(h2h.balance),0,h2h.balance) ) %>% 
        return()
    },
    .h2ht=h2h.table)
  ) %>% 
  unnest(data) %>% 
  arrange(week, desc(wins), desc(h2h.balance), desc(pts.pro), pts.ag) %>% 
  mutate( wrank = rep(1:10,4) ) %>% 
  select(-op.id, -op.team, -op.out)
  
wrank %>%
  ggplot(aes(x=week, y=pts, group=team)) +
  geom_line(aes(color=team), size=1) +
  geom_image(aes(image=logoUrl)) +
  ggtitle("Pontuação dos Times", "por semana") +
  theme_minimal()

wrank %>% 
  ggplot(aes(x=week, y=pts.pro, group=team)) +
  geom_line(aes(color=team), size=1) +
  geom_image(aes(image=logoUrl), size=.03) +
  ggtitle("Pontuação Acumulada dos Times", "semana à semana") +
  theme_minimal()


wrank %>% 
  ggplot(aes(x=week, y=reorder(wrank,-wrank), group=team)) +
  geom_line(aes(color=team), size=2) +
  geom_image(aes(image=logoUrl), size=.04) +
  geom_hline(yintercept = 6.5, linetype="dashed", size=1, color="darkgrey") +
  geom_text(x=4.4, y=6.65, label="playoff clinch", size=3, color="darkgrey") +
  ylab("rank") +
  ggtitle("Weekly Ranking") +
  theme_void()

