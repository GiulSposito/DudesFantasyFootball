# lendo liga e token do yaml (para n?o versionar o access token)
library(yaml)
config <- yaml.load_file("./config/config.yml")
leagueId <- config$leagueId
authToken <- config$authToken

library(tidyverse)
source("./R/api/ffa_league.R")

resp <- ffa_league_matchups(authToken, leagueId, 4)

json <- resp$content

teams <- json$games$`102019`$leagues$`3940933`$teams

teams %>% 
  map_df(function(.team){
    team$rosters[[1]] %>% 
      bind_rows() %>%
      return()
  }, .id="teamId") %>% 
  as_tibble() %>% 
  mutate_at(vars(teamId, rosterSlotId, playerId), as.integer)

