library(yaml)
library(tidyverse)
source("./R/api/ffa_league.R")

.week  <- 2
config <- yaml.load_file("./config/config.yml")

teams <- ffa_league_teams(config$authToken, config$leagueId) %>% 
  ffa_extractTeams()

recapResps <- teams$teamId %>% 
  map(~ffa_league_matchups_recap(config$authToken, config$leagueId, .week=.week, .teamId = .x))

recaps <- recapResps %>% 
  map_df(ffa_extractRecap) %>% 
  distinct()

recaps %>% 
  split(1:nrow(.)) %>% 
  map(function(.recap){
    cat(paste0("### ", .recap$title), "\n\n")
    .recap$paragraphs %>% map(function(.par){
      .par$text %>% paste0("\n\n") %>% cat()
    })
  })

