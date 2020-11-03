library(tidyverse)
library(lubridate)
library(glue)
library(ffanalytics)
library(flexdashboard)
library(yaml)
library(broom)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# EXECUTION PARAMETERS ####
week <- 7
season <- 2020
config <- read_yaml("./config/config.yml")
prefix <- "posTNF"
destPath <- "static/reports/2020"
sim.version <- 5

source("./R/api/ffa_players.R")
players_stats <- ffa_players_stats(config$authToken, config$leagueId, season, 1:week) %>% 
  ffa_extractPlayersStats()

# carregando tabelas de "de para" de IDs de Jogadores
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
my_player_ids <- player_ids %>%
  mutate(
    id = as.integer(id), 
    nfl_id = as.integer(nfl_id)) %>%
  as_tibble() %>% 
  # completa a tabela de mapeamento de projecoes do ffanalytics
  bind_rows(readRDS("./data/playerIds_not_mapped.rds"))

# calcula tabela de pontuacao para todos os jogadores usa na simulacao
source("./R/simulation/players_projections.R")
site_ptsproj <- calcPointsProjection(season, read_yaml("./config/score_settings.yml"))
pts_errors <- projectErrorPoints(players_stats, site_ptsproj, my_player_ids, week)

# pts_flcl <- projectFloorCeiling(proj_table, week, season)


# adiciona os erros de projeções passadas
ptsproj <- site_ptsproj %>% # projecao dos sites
  bind_rows(pts_errors) 

x <- players_stats %>% 
  select(playerId, weekPts) %>% 
  unnest(weekPts) %>% 
  inner_join(select(my_player_ids, id, playerId=nfl_id), by="playerId") %>% 
  inner_join(ptsproj, by=c("id","week")) %>% 
  select(playerId, pos, week, data_src, y=weekPts, y_hat=pts.proj) %>% 
  filter(week<7) %>% 
  group_by(data_src, pos, week) %>% 
  nest() %>% 
  mutate( model = map(data, function(.d){ lm(y_hat~y, data=.d) })) %>% 
  mutate( tidy = map(model, function(.m){ tidy(.m) })) %>% 
  mutate( augm = map(model, function(.m){ augment(.m) })) %>% 
  mutate( glan = map(model, function(.m){ glance(.m) })) %>% 
  mutate( estimates = map(tidy, function(.t){
    .t %>% 
      select(term, estimate) %>% 
      pivot_wider(names_from = "term", values_from="estimate") %>% 
      return()
  })) %>% 
  unnest(c(glan, estimates)) %>% 
  janitor::clean_names()

x %>% 
  ggplot(aes(y, r_squared, color=data_src)) +
  geom_point() +
  theme_minimal() +
  facet_grid(pos ~ week)



