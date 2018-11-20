library(tidyverse)
library(yaml)
library(httr)
library(jsonlite)
library(glue)

# lendo liga e token do yaml (para n?o versionar o access token)
config <- yaml.load_file("./config/config.yml")
leagueId <- config$leagueId
authToken <- config$authToken

# contexto da semana
week <- 12

# obtem os matchups
url.matchups <- "http://api.fantasy.nfl.com/v1/league/matchups?leagueId={leagueId}&week={week}&format=json&authToken={authToken}"

# faz a chamada na api
httr::GET(glue(url.matchups)) -> resp

resp %>% 
  httr::content(as="text") %>%
  fromJSON(simplifyDataFrame = T) -> matchups.json

# processa o json
matchups <- matchups.json$leagues$matchups[[1]] %>% 
  jsonlite::flatten() %>% 
  as.tibble()

# para cada um dos times, faz a chamada para pegar os times escalados no matchup
url.team.matchup <- "http://api.fantasy.nfl.com/v1/league/team/matchup?leagueId={leagueId}&teamId={teamId}&week={week}&authToken={authToken}&format=json"

matchup.teams.json <- matchups$awayTeam.id %>% 
  map(function(teamId, .url){
    httr::GET(glue(.url)) %>% 
      httr::content(as = "text") %>% 
      jsonlite::fromJSON(simplifyDataFrame = T) %>% 
      return()
  },
  .url=url.team.matchup)

saveRDS(matchup.teams.json, glue("./data/week{week}_matchups_json.rds"))
