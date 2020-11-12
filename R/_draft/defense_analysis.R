library(tidyverse)
library(jsonlite)
library(httr)

sim <- readRDS("./data/simulation_v5_week8_final.rds")

defs <- sim$players_stats %>% 
  filter(position=="DEF")

defs[1,]$weekStats[[8]]

defs[1,]$stats[[1]]$season[[1]] %>%
  tibble(
    id = names(.),
    value  = unlist(.)
  ) %>% 
  select(-1) %>% 
  left_join(stat_ids, by="id") %>% 
  View()

## stats id

resp <- GET("https://api.fantasy.nfl.com/v2/game/stats?appKey=internalemailuse") %>%
  content(as="text")

json_stat_id <- resp %>%
  jsonlite::fromJSON(flatten = T, simplifyDataFrame = T)

stat_ids <- json_stat_id$games$`102020`$stats %>%
  map_df(function(.stat){
    .stat %>%
      keep(~!is.null(.x)) %>%
      as_tibble()
  })
