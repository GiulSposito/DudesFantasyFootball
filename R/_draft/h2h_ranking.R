library(tidyverse)
library(comperank)
library(glue)

games <- 1:8 %>% 
  map_df(function(.w){
    sim <- readRDS(glue("./data/simulation_v5_week{.w}_final.rds"))
    
    matchups <- sim$matchups %>% 
      select(game=matchupId, player1=awayTeam.teamId, player2=homeTeam.teamId)
    
    scores <- sim$teams %>% 
      select(teamId, name, week.stats) %>% 
      unnest(week.stats) %>% 
      select(-week)
    
    games <- scores %>% 
      select(player1=teamId, score1=pts) %>% 
      inner_join(matchups, by="player1")
    
    scores %>% 
      select(player2=teamId, score2=pts) %>% 
      inner_join(games, by="player2") %>% 
      select(game, player1, score1, player2, score2) %>% 
      return()
  })

games %>% 
  distinct() %>% 
  arrange(game) %>% 
  as_widecr() %>% 
  rank_massey() %>% 
  arrange(ranking_massey) %>% 
  inner_join(select(sim$teams, teamId, name), by=c("player"="teamId"))
  
  
  mutate(
    game=row_number(),
    player1=as.character(player1), 
    player2=as.character(player2)) %>%
  
  rank_massey()
  
  
  h2h_funs

ncaa2005 %>% 
  comperes::
