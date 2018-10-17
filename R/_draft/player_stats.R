library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(yaml)
library(glue)


getPlayersPts <- function( .week, .season, .leagueId, .authToken){
  url <- glue("http://api.fantasy.nfl.com/v1/league/players?leagueId={.leagueId}&format=json&statSeason={.season}&statWeek={.week}&authToken={.authToken}&count=500")
  httr::GET(url) %>% 
    httr::content(as = "text") %>% 
    jsonlite::fromJSON(simplifyDataFrame = T) %$%
    leagues %$%
    players %>% 
    .[[1]] %>% 
    select(id, firstName, lastName, position, teamAbbr, fantasyPts) %>% 
    jsonlite::flatten() %>% 
    as.tibble() %>% 
    filter(fantasyPts.week.pts!=0) %>% 
    return()
}

config <- yaml.load_file("./config/config.yml")
leagueId  <- config$leagueId
authToken <- config$authToken
season <- 2018
weeks <- 1:6

weeks %>% 
  map(
    getPlayersPts,
    .season   = season,
    .leagueId = leagueId,
    .authToken = authToken
  ) %>% 
  bind_rows() %>% 
  rename(src_id=id) -> player.points

saveRDS(player.points,"./data/players_points.rds")
