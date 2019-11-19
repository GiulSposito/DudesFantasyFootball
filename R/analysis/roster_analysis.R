library(tidyverse)
library(glue)

idealRoster <- function(.teamName, .week, .useFA=T){
  
  # seleciona se vai otimizar TIME+FA ou sO TIME
  selTeams <- ifelse(.useFA, c("*FreeAgent",.teamName), .teamName)
  
  # jogadores disponiveis para otimizacao
  players <- readRDS(glue("./data/week{.week}_players_projections.rds")) %>% 
    filter(
      fantasy.team %in% selTeams,
      !(team %in% c("FA", "FA*"))
    ) 

  # starters: slots so com um tipo de jogador
  starters <- tibble(
    pos=c("QB","RB","WR","TE","K","DST"),
    qtd=c(1,2,2,1,1,1)
  ) %>% 
    split(1:nrow(.)) %>% 
    map_df(function(.x, .players){
      .players %>% 
        filter(position==.x$pos) %>%
        top_n(.x$qtd, floor)
    }, .players=players)
  
  # starters: slot que pode ser tando RB quando WR 
  starters <- players %>% 
    filter(pos %in% c("WR","RB")) %>% 
    filter(is.na(injuryGameStatus)) %>% 
    anti_join(starters) %>% 
    top_n(1, floor) %>% 
    bind_rows(starters,.)
  
  ## bench
  bench <- tibble(
    pos=c("QB","RB","WR","TE"),
    qtd=c(1,2,2,1)
  ) %>% 
    split(1:nrow(.)) %>% 
    map_df(function(.x, .players){
      .players %>% 
        filter(position==.x$pos) %>% 
        top_n(.x$qtd, floor)
    }, .players = anti_join(players, starters) )
  
  # releases
  releases <- players %>% 
    filter(fantasy.team==.teamName) %>% 
    anti_join(starters) %>% 
    anti_join(bench)
  
  # new contracts
  starters %>% 
    bind_

  list(
    starters=starters,
    bench=bench,
    releases=releases
  )        
}

starters
bench 
releases

