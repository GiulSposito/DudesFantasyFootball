library(tidyverse)

sim <- readRDS("./data/simulation_v5_week3_posMNF.rds")

.week <- 3

rosters <- sim$teams %>% 
  select(teamId, teamName=name, teamImg=imageUrl, rosters) %>% 
  unnest(rosters)
  
players <- sim$players_stats %>% 
  select(playerId, playerName=name, nflTeam=nflTeamAbbr, playerImg=smallImageUrl, position, weekPts) %>% 
  unnest(weekPts) %>% 
  filter(week==.week) %>% 
  left_join(rosters, by="playerId") %>% 
  select(playerId, position, playerName, nflTeam, points=weekPts, fantasyTeam=teamName, playerImg, teamImg)
  

findBestTeam <- function(.players){

  best_teamPlayers <- tibble(
    pos=c("QB","RB","WR","TE","K","DEF"),
    qtd=c(1,2,2,1,1,1)
  ) %>% 
    split(1:nrow(.)) %>% 
    map_df(function(.x, .plrs){
      .plrs %>%
        filter(position==.x$pos) %>%
        top_n(.x$qtd, points) %>%
        return()
    }, .plrs=.players)
  
  best_teamPlayerWR <- .players %>% 
    filter(position %in% c("RB","WR")) %>% 
    anti_join(best_teamPlayers, by="playerId") %>% 
    top_n(1, points) %>% 
    mutate( position = paste0("W/R (",position,")"))
  
  best_teamPlayers %>% 
    bind_rows(best_teamPlayerWR) %>% 
    return()
}


players %>% 
  findBestTeam() %>% 
  pull(points) %>% 
  sum()

players %>% 
  filter(is.na(fantasyTeam)) %>% 
  findBestTeam()
