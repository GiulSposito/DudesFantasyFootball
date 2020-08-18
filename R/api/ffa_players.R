library(glue)
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

