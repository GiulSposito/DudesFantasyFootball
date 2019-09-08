library(tidyverse)
library(yaml)
library(httr)
library(jsonlite)
library(glue)

checkFantasyAPI <- function(.week){
  
  # lendo liga e token do yaml (para n?o versionar o access token)
  config <- yaml.load_file("./config/config.yml")
  leagueId <- config$leagueId
  authToken <- config$authToken
  
  # obtem os matchups
  url.matchups <- "http://api.fantasy.nfl.com/v1/league/matchups?leagueId={leagueId}&week={week}&format=json&authToken={authToken}"
  
  # faz a chamada na api
  resp <- httr::GET(glue(url.matchups, week=.week))
  
  return(resp$status_code==200)
  
}

