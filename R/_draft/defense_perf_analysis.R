library(tidyverse)
library(httr)
library(jsonlite)

sim <- readRDS("./data/simulation_v5_week16_final.rds")

players <- sim$players_stats %>% 
  filter(position=="DEF") %>% 
  select(playerId, nflTeamAbbr, stats)

stats <- players %>% 
  unnest(stats) %>% 
  mutate( len = map_int(stats, length) ) %>% 
  filter( len!=0 ) %>% 
  select( -len ) %>% 
  mutate( stats.df = map(stats, function(.x){
    tibble(
      id = names(.x$`2020`),
      value   = as.numeric(.x$`2020`)
    )    
  })) %>% 
  unnest(stats.df)



resp <- GET("https://api.fantasy.nfl.com/v2/game/stats?appKey=internalemailuse") %>%
  content(as="text")

stid <- resp %>% 
  fromJSON()

stid <- stid$games$`102020`$stats %>% 
  map(discard, is.null) %>% 
  map_df(as_tibble)


stats %>% 
  inner_join(stid, by="id")
