install.packages("comperank")
library(tidyverse)

gm_res <- readRDS("./data/post_matchups_results.rds")

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
  ) %>% 
  mutate(week=as.integer(week))