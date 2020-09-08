library(jsonlite)
library(httr)
library(tidyverse)
library(glue)

recap_list <- 1:14 %>% 
  map(function(x){
    GET(glue("https://nfl-fantasy.automatedinsights.com/content-prod/2020/draftrecap/eb70/3940933/{x}.json.gzip")) %>% 
      content(as="text") %>% 
      str_remove("YUI.Env.onDraftRecapLoaded\\(  ") %>% 
      str_remove("  \\);") %>% 
      fromJSON(flatten = T, simplifyDataFrame = T)
  })


recap <- recap_list %>% 
  map_df(function(x){
    tibble(
      text = x$paragraphs$text,
      probs = list(as_tibble(x$probabilities)),
      proj  = list(as_tibble(x$projected_outcomes))
    )
  }) %>% 
  unnest(c(probs, proj)) %>% 
  mutate( teamId = 1:14 ) %>% 
  select( teamId, everything())

  

source("./R/api/ffa_league.R")
config <- yaml::read_yaml("./config/config.yml")
teams_resp <- ffa_league_teams(config$authToken,config$leagueId)

teams <- teams_resp$content$games[[1]]$leagues[[1]]$teams %>% 
  map_df(function(x){
    tibble(
      teamId = as.integer(x$teamId),
      teamName = x$name
    )
  })


draft_recap <- teams %>% 
  inner_join(recap)

library(googlesheets4)
googlesheets4::gs4_create("Draft Ranking", sheets = list(draft_grades=draft_recap))
