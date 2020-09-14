library(tidyverse)
library(jsonlite)
library(httr)
library(ffanalytics)

# parameters

week <- 1
season <- 2020

source("./R/import/ffa_player_projection.R")
scraps <- scrapPlayersPredictions(week, season, T)


# carregando tabelas de "de para" de IDs de Jogadores
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
miss_ids <- readRDS("./data/playerIds_not_mapped.rds") %>% 
  mutate( 
    id = as.character(id), 
    nfl_id = as.character(nfl_id)
  )

player_ids <- player_ids %>%
  # completa a tabela de mapeamento de projecoes do ffanalytics
  bind_rows(miss_ids) %>% 
  as_tibble()




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

# PLAYERS
config <- yaml::read_yaml("./config/config.yml")
source("./R/api/ffa_players.R")
players_stats <- ffa_players_stats(config$authToken, config$leagueId, season, 1:week) %>% 
  ffa_extractPlayersStats()

nflplayers <- players_stats %>% 
  select(src_id=playerId, player=name, position, team=nflTeamAbbr) %>% 
  mutate(
    src_id=as.character(src_id),
    team=if_else(team=="",as.character(NA), team)
  )

## Exploring a way to join

## NFL Proj Col Especification
ffa_columns = tibble(
    colName = names(projection_sources[["NFL"]]$stat_cols),
    shortName = projection_sources[["NFL"]]$stat_cols
  )


## gerando uma tabela igual da FFA
nfl_scrap <- fantasyProj %>%
  inner_join(ffa_columns, by = "shortName") %>%
  mutate(value=as.numeric(value)) %>%
  select(src_id=playerId, colName, value) %>%
  filter(complete.cases(.)) %>%
  distinct() %>%
  pivot_wider(id_cols=src_id, names_from=colName, values_from=value) %>% 
  inner_join(select(player_ids, id, nfl_id), by=c("src_id"="nfl_id")) %>% 
  inner_join(nflplayers, by="src_id") %>% 
  mutate(
    data_src="NFL",
    week=as.character(week),
    position = if_else(position=="DEF","DST",position)
  ) %>% 
  select(data_src, id, src_id, player, position, team, everything())

  
## NFL Proj Col ID
# player_cols = c(src_id = "id", player = "name", team = "teamabbr", pos = "position")

# scrp <- scrape_data(src="CBS",season = 2020, week = 1, pos = c("QB","RB","WR"))
# scrp <- readRDS("./data/week1_scrap.rds")
# 
# scrp[[1]] <- scrp[[1]] %>% 
#   bind_rows(filter(nfl_scrap, position=="QB"))
# 
# scrp[[1]] %>% View()
# projections_table(scrp,  yaml::read_yaml("./config/score_settings.yml"))

scraps

names(scraps) %>% 
  map(function(pos, .scrp=scraps, .nfl=nfl_scrap){
    pos_col <- intersect(names(.scrp[[pos]]),names(nfl_scrap))
    nfl_scrap %>% 
      filter(position==pos) %>% 
      select(pos_col) %>% 
      bind_rows(.scrp[[pos]])
  }) %>% 
  set_names(names(scraps)) -> chg_scraps

attr(chg_scraps, "season") <- attr(scraps,"season")
attr(chg_scraps, "week") <- attr(scraps,"week")


projections_table(chg_scraps,  yaml::read_yaml("./config/score_settings.yml")) %>% 
  arrange(desc(points))

projections_table(scraps,  yaml::read_yaml("./config/score_settings.yml")) %>% 
  arrange(desc(points))

scraps[[1]]$week
chg_scraps[[1]] %>% names()
