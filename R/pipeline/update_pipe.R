library(tidyverse)
library(lubridate)
library(glue)
library(ffanalytics)
library(flexdashboard)
library(yaml)
library(tidyverse)
library(lubridate)
library(glue)
library(ffanalytics)
library(flexdashboard)
library(yaml)

# EXECUTION PARAMETERS ####
week <- 2
season <- 2020
config <- read_yaml("./config/config.yml")
prefix <- "posMNF"
destPath <- "static/reports/2020"
sim.version <- 5

# API ACCESS CHECK ####
source("./R/import/checkFantasyAPI.R")
if(!checkFantasyAPI(config$authToken, config$leagueId, week)) stop("Unable to access Fantasy API!")

# TABELA DE PROJECAO ####
source("./R/import/ffa_player_projection.R")
scraps <- readRDS(glue("./data/week{week}_scrap.rds"))
proj_table  <- calcPlayersProjections(scraps, read_yaml("./config/score_settings.yml"))

# PLAYERS AND MATCHUPS ####
# PLAYERS
source("./R/api/ffa_players.R")
players_stats <- ffa_players_stats(config$authToken, config$leagueId, season, 1:week) %>% 
  ffa_extractPlayersStats()

# MATCHUPS
source("./R/api/ffa_league.R")
leagueMatchups <- ffa_league_matchups(config$authToken, config$leagueId, week)
matchups_games <- ffa_extractMatchups(leagueMatchups)
teams_rosters  <- ffa_extractTeamsFromMatchups(leagueMatchups)   

# carregando tabelas de "de para" de IDs de Jogadores
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
my_player_ids <- player_ids %>%
  mutate(
    id = as.integer(id), 
    nfl_id = as.integer(nfl_id)) %>%
  as_tibble() %>% 
  # completa a tabela de mapeamento de projecoes do ffanalytics
  bind_rows(readRDS("./data/playerIds_not_mapped.rds"))


# SIMULACAO ####

# calcula tabela de pontuacao para todos os jogadores usa na simulacao
source("./R/simulation/players_projections.R")
site_ptsproj <- calcPointsProjection(season, read_yaml("./config/score_settings.yml"))
pts_errors <- projectErrorPoints(players_stats, site_ptsproj, my_player_ids, week)
pts_flcl <- projectFloorCeiling(proj_table, week, season)

# adiciona os erros de projeções passadas
ptsproj <- site_ptsproj %>% # projecao dos sites
  bind_rows(pts_errors) %>% # erros das projecoes nas semanas passadas
  bind_rows(pts_flcl)       # floor e ceiling da projecao dos sites

# simulação das partidas
source(glue("./R/simulation/points_simulation_v{sim.version}.R"))
sim <- simulateGames(week, season, ptsproj, matchups_games, teams_rosters, players_stats, my_player_ids, proj_table)

# salva resultado
saveRDS(sim, glue("./data/simulation_v{sim.version}_week{week}_{prefix}.rds"))

# # constroi o relatório
rmarkdown::render(
  input = glue("./R/reports/dudes_simulation_v{sim.version}.Rmd"),
  output_file = glue("../../{destPath}/dudes_simulation_v{sim.version}_week{week}_{prefix}.html"),
  output_format = "flex_dashboard",
  params = list(week=week, prefix=prefix)
)


