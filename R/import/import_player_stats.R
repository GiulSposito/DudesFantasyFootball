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
    # select(id, firstName, lastName, position, teamAbbr, fantasyPts) %>%
    select( -weekStats, -seasonStats ) %>% 
    rename(team=teamAbbr) %>% #, x=fantasyPts) %>% 
    jsonlite::flatten()  %>%  
    # select(-x.season.season, -x.season.pts) %>% 
    mutate(season=fantasyPts.week.season, week=fantasyPts.week.week, points=fantasyPts.week.pts) %>% 
    as_tibble() %>%
    mutate_at(vars(id, season, week), as.integer) %>% 
    mutate_at(vars(points), as.numeric) %>% 
    mutate( src_id=id ) %>% 
    return()
}

importPlayerStatistics <- function(.weeks, .saveToFile=T){
  
  # api basic access
  config <- yaml.load_file("./config/config.yml")
  leagueId  <- config$leagueId
  authToken <- config$authToken
  season <- config$season

  # for each week.. 
  player.points <- .weeks %>% 
    map(
      getPlayersPts,
      .season   = season,
      .leagueId = leagueId,
      .authToken = authToken
    ) %>% 
    bind_rows()
  
  player_points_proj <-  player.points %>% 
    rename( nfl_id = id ) %>%
    inner_join(player_ids, by=c("nfl_id"="nfl_id")) %>%
    # inner_join(player_ids, by=c("id"="nfl_id")) %>% 
    as_tibble()
  
  if (.saveToFile) saveRDS(player_points_proj, "./data/players_points.rds")
  
  return(player_points_proj)
}




