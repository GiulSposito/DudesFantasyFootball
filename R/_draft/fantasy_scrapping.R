library(tidyverse)
library(jsonlite)
library(httr)
library(ffanalytics)

# Fantasy Projections #####

## Statistics IDs

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

## Week Projections

resp <- GET("https://api.fantasy.nfl.com/v2/players/weekprojectedstats?season=2020&week=1") %>%
  content(as="text")

json_stats <- resp %>%
  fromJSON(flatten = T, simplifyDataFrame = T)

stats <- json_stats$games$`102020`$players %>%
  tibble(id=names(.),
         values=.) %>%
  mutate(values = map(values, function(.x){
    # id$projectedStats$week$2020$1$stats
    .x[[1]][[1]][[1]][[1]] %>%
      as_tibble()
  })) %>%
  unnest(values)

## join stats ID with Projections

fantasyProj <- stats %>%
  pivot_longer(cols=c(-id), names_to="stat_id") %>%
  rename(playerId=id) %>%
  left_join(stat_ids, by=c("stat_id"="id"))

## Exploring a way to join

## NFL Proj Col Especification
ffa_columns = tibble(
    colName = names(projection_sources[["NFL"]]$stat_cols),
    shortName = projection_sources[["NFL"]]$stat_cols
  )


# carregando tabelas de "de para" de IDs de Jogadores
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
player_ids <- player_ids %>%
  mutate(
    id = as.integer(id), 
    nfl_id = as.integer(nfl_id)) %>%
  as_tibble() %>% 
  # completa a tabela de mapeamento de projecoes do ffanalytics
  bind_rows(readRDS("./data/playerIds_not_mapped.rds"))


## gerando uma tabela igual da FFA
fantasyProj %>%
  inner_join(ffa_columns, by = "shortName") %>%
  mutate(value=as.numeric(value)) %>%
  select(src_id=playerId, colName, value) %>%
  filter(complete.cases(.)) %>%
  distinct() %>%
  pivot_wider(id_cols=src_id, names_from=colName, values_from=value)

player_ids %>% 
  select(id, nfl_id)
  
## NFL Proj Col ID
projection_sources[["NFL"]]$player_cols = c(src_id = "id", player = "name", team = "teamabbr", pos = "position")

  
scrp <- ffanalytics::scrape_data(src="CBS",season = 2020, week = 1, pos = c("QB","RB","WR"))

scrp[[1]] %>% names()
scrp[[2]] %>% names()
scrp[[3]] %>% names()

projection_sources[["NFL"]]

ff_player_data
