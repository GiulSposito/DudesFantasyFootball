library(glue)
library(tidyverse)
source("./R/api/ffa_api.R")


# return the league players
ffa_players <- function(.authToken, .leagueId){
  
  players <- ffa_api(
    .path = "v2/league/players",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "leagueId"  = .leagueId,
      "count"     = 2000
    ))
  
}

# return the league player with stats
ffa_players_stats <- function(.authToken, .leagueId, .season, .weeks){

  # # stats reference
  # 
  # [
  #   {"type":"researchStats","season":"2015","week":"1"},
  #   {"type":"ranks","season":"2015","week":"1"},
  #   {"type":"stats","season":"2015","week":"1"},
  #   {"type":"stats","season":"2015"},
  #   {"type":"projectedStats","season":"2015","week":"1"},
  #   {"type":"nflGames","season":"2015","week":"1"}
  # ]

  
  # define estatísticas para retorno
  stats_str <- .weeks %>% 
    map_chr(~glue('{"type":"stats","season":"<<.season>>","week":"<<.x>>"}', .open = "<<", .close = ">>")) %>% 
    c(.,glue('{"type":"stats","season":"<<.season>>"}', .open = "<<", .close = ">>")) %>% 
    paste(collapse = ",") %>% 
    paste0("[",.,"]")
  
    
  players <- ffa_api(
    .path = "v2/league/players",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "leagueId"  = .leagueId,
      "stats"     = stats_str,
      "count"     = 2000
    ))
  
}


# convert uma resposta em um dataframe
ffa_extractPlayersStats <- function(playersStatsResp){
  
  playersStatsResp$content$games[[1]]$players %>% 
    tibble(players=.) %>% 
    unnest_wider(players) %>% 
    hoist(stats, seasonPts = c(1, 1, "pts")) %>% 
    hoist(stats, weekStats = c(1, 1)) %>% 
    mutate( weekPts = map(weekStats, function(wp){
      wp %>% 
        map(~pluck(.x, "pts", .default = NA)) %>% 
        unlist(.) %>% 
        tibble(
          week = as.integer(names(.)),
          weekPts  = as.numeric(.),
        ) %>% 
        arrange(week) %>% 
        mutate(
          weekSeasonPts = if_else(is.na(weekPts), 0, weekPts),
          weekSeasonPts = cumsum(weekSeasonPts)) %>% 
        select(week, weekPts, weekSeasonPts)
    })) %>% 
    mutate(across(c(playerId, nflTeamId, byeWeek), as.integer)) %>% 
    return()
  
}

