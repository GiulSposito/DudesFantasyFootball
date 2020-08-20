library(yaml)
library(tidyverse)
source("./R/api/ffa_league.R")
source("./R/api/ffa_players.R")

config <- yaml.load_file("./config/config.yml")


##### MATCHUPS, TEAMS AND ROSTERS
leagueMatchups <- ffa_league_matchups(.authToken = config$authToken, .leagueId = config$leagueId, .week=1)

leagueMatchups$content$games[[1]]$leagues[[1]]$teams[[1]]
