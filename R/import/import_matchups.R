library(tidyverse)
library(yaml)
library(httr)
library(jsonlite)
library(glue)

source("./R/api/ffa_league.R")

importMatchups <- function(.week, .saveToFile=T){
  
  # lendo liga e token do yaml (para n?o versionar o access token)
  config <- yaml.load_file("./config/config.yml")
  leagueId <- config$leagueId
  authToken <- config$authToken
  
  # obtem os matchups
  url.matchups <- "http://api.fantasy.nfl.com/v1/league/matchups?leagueId={leagueId}&week={.week}&format=json&authToken={authToken}"
  
  # faz a chamada na api
  httr::GET(glue(url.matchups)) -> resp
  
  resp %>% 
    httr::content(as="text") %>%
    fromJSON(simplifyDataFrame = T) -> matchups.json
  
  # processa o json
  matchups <- matchups.json$leagues$matchups[[1]] %>% 
    jsonlite::flatten() %>% 
    as_tibble()
  
  # para cada um dos times, faz a chamada para pegar os times escalados no matchup
  url.team.matchup <- "http://api.fantasy.nfl.com/v1/league/team/matchup?leagueId={leagueId}&teamId={teamId}&week={.week}&authToken={authToken}&format=json"
  
  matchup.teams.json <- matchups$awayTeam.id %>% 
    map(function(teamId, .url){
      httr::GET(glue(.url)) %>% 
        httr::content(as = "text") %>% 
        jsonlite::fromJSON(simplifyDataFrame = T) %>% 
        return()
    },
    .url=url.team.matchup)
  
  if (.saveToFile) saveRDS(matchup.teams.json, glue("./data/week{.week}_matchups_json.rds"))

  return(matchup.teams.json)  
}

# busca na nova api de rosters o status de game do jogador (se ele já jogou)
importPlayerGameStatus <- function(.week, .player_ids){
  
  #configuracoes de acesso
  .config    <- yaml.load_file("./config/config.yml")
  .leagueId  <- .config$leagueId
  .authToken <- .config$authToken
  
  # chama a API
  resp <- ffa_league_matchups(.authToken, .leagueId, .week)

  # obtem a relacao de times
  teams <- resp$content$games$`102019`$leagues[[.leagueId]]$teams
  
  # percorre os rosters indo atras da informacao sobre "Editavel"
  playerGameStatus <- map_df(teams, function(.team){
      .team$rosters[[1]] %>% 
        bind_rows() %>%
        return()
    }, .id="teamId") %>% 
    as_tibble() %>% 
    mutate_at(vars(teamId, rosterSlotId, playerId), as.integer) %>% 
    mutate( played = !isEditable ) %>%  # cria uma flag para saber se o jogador 
    select(teamId, playerId, played) %>% 
    return()
  
  # casa os IDs e retorna
  .player_ids %>% 
    select(id, nfl_id) %>%
    inner_join(playerGameStatus, by=c("nfl_id"="playerId")) %>% 
    return()
  
}
