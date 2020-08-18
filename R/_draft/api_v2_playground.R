library(yaml)
library(tidyverse)
source("./R/api/ffa_league.R")
source("./R/api/ffa_players.R")

config <- yaml.load_file("./config/config.yml")


##### MATCHUPS, TEAMS AND ROSTERS
leagueMatchups <- ffa_league_matchups(.authToken = config$authToken, .leagueId = config$leagueId, .week=1)


ffa_extractTeams <- function(ffaLeagueMatchups){
  
  # extract teams and rosters
  ffaLeagueMatchups$content$games[[1]]$leagues[[1]]$teams %>% 
    tibble(team=.) %>% 
    unnest_wider(team) %>% 
    mutate(across(c(teamId, ownerUserId), as.integer)) %>% 
    select(-matchups, -stats) %>% 
    mutate( rosters = map(rosters, function(r){
      r[[1]] %>% 
        bind_rows(.id="position") %>% 
        as_tibble() %>% 
        mutate(across(rosterSlotId:playerId,as.integer)) %>% 
        return()
    })) %>% 
    return()
  
}


ffa_extractMatchus <- function(ffaLeagueMatchups){
  
  # extract matchups
  ffaLeagueMatchups$content$games[[1]]$leagues[[1]]$matchups %>% 
    tibble(matchups=.) %>% 
    unnest_wider(matchups) %>% 
    unnest_wider(awayTeam, names_sep=".") %>% 
    unnest_wider(homeTeam, names_sep=".") %>% 
    mutate(across(c(week, ends_with("teamId")), as.integer)) %>% 
    return()
  
}


ffa_extractMatchus(leagueMatchups)
ffa_extractTeams(leagueMatchups)

##### PLAYERS STATS
playersStats <- ffa_players_stats(config$authToken, config$leagueId, 2019, 1:5)

ffa_extractPlayersStats <- function(playersStatsResp){
  
  playersStatsResp$content$games[[1]]$players %>% 
    tibble(players=.) %>% 
    unnest_wider(players) %>% 
    hoist(stats, seasonPts = c(1, 1, "pts")) %>% 
    hoist(stats, weekPts = c(1, 1)) %>% 
    mutate( weekPts = map(weekPts, function(wp){
      wp %>% 
        map(~pluck(.x, "pts", .default = NA)) %>% 
        unlist(.) %>% 
        tibble(
          week = names(.),
          pts  = as.numeric(.)
        )
    })) %>% 
    mutate(across(c(playerId, nflTeamId, byeWeek), as.integer)) %>% 
    mutate(across(seasonPts,as.numeric)) %>% 
    return()

}

ffa_extractPlayersStats(playersStats)
