library(yaml)
library(tidyverse)
source("./R/api/ffa_league.R")
source("./R/api/ffa_players.R")

config <- yaml.load_file("./config/config.yml")


##### MATCHUPS, TEAMS AND ROSTERS
matchups <-ffa_league_matchups(.authToken = config$authToken, .leagueId = config$leagueId, .week=1)

# extract teams and rosters
matchups$content$games[[1]]$leagues[[1]]$teams %>% 
  tibble(team=.) %>% 
  unnest_wider(team)

# extract matchups
matchups$content$games[[1]]$leagues[[1]]$matchups %>% 
  tibble(matchups=.) %>% 
  unnest_wider(matchups) %>% 
  unnest_wider(awayTeam, names_sep=".") %>% 
  unnest_wider(homeTeam, names_sep=".")


##### PLAYERS STATS
players <- ffa_players_stats(config$authToken, config$leagueId, 2019, 1:5)

.season <- 2019

players$content$games[[1]]$players %>% 
  tibble(players=.) %>% 
  unnest_wider(players) %>% 
  hoist(stats, seasonPts = c("season", .season, "pts")) %>% 
  hoist(stats, weekPts = c("week", .season)) %>% 
  mutate( weekPts = map(weekPts, function(wp){
    wp %>% 
      map(~pluck(.x, "pts", .default = NA)) %>% 
      unlist(.) %>% 
      tibble(
        week = names(.),
        pts  = .
      )
  }))



