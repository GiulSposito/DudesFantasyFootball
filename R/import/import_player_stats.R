library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(yaml)
library(glue)


getPlayersPts <- function( .week, .season, .leagueId, .authToken){
  url <- glue("http://api.fantasy.nfl.com/v1/league/players?leagueId={.leagueId}&format=json&statSeason={.season}&statWeek={.week}&authToken={.authToken}&count=500")
  httr::GET(url) -> resp
  
  resp %>% 
    httr::content(as = "text") %>% 
    jsonlite::fromJSON(simplifyDataFrame = T) %$%
    leagues %$%
    players %>% 
    .[[1]] %>% 
    select(id, firstName, lastName, position, teamAbbr, fantasyPts) %>% 
    rename(team=teamAbbr, x=fantasyPts) %>% 
    jsonlite::flatten() %>% 
    select(-x.season.season, -x.season.pts) %>% 
    rename(season=x.week.season, week=x.week.week, points=x.week.pts) %>% 
    as.tibble() %>%
    mutate_at(vars(id, season, week), as.integer) %>% 
    mutate_at(vars(points), as.numeric) %>% 
    rename( src_id=id ) %>% 
    return()
}

config <- yaml.load_file("./config/config.yml")
leagueId  <- config$leagueId
authToken <- config$authToken
season <- 2018
weeks <- 1:13

weeks %>% 
  map(
    getPlayersPts,
    .season   = season,
    .leagueId = leagueId,
    .authToken = authToken
  ) %>% 
  bind_rows() -> player.points

readRDS("./data/nfl_players_id.rds") %>% 
  select(id, src_id) %>% 
  inner_join(player.points, by="src_id") %>% 
  saveRDS("./data/players_points.rds")
