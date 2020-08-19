# library(tidyverse)
# library(lubridate)
# library(glue)
# library(ffanalytics)
# library(flexdashboard)
library(yaml)

# EXECUTION PARAMETERS ####
week <- 1
season <- 2019
config <- yaml::read_yaml("./config/config.yml")
# sim.version <- 3
# prefix <- "preDraft"
# destPath <- "static"

# API ACCESS CHECK ####
source("./R/import/checkFantasyAPI.R")
if(!checkFantasyAPI(config$authToken, config$leagueId, week)) stop("Unable to access Fantasy API!")

# SCRAPPING AND PROJECTION ####
source("./R/import/ffa_player_projection.R")
scraps <- scrapPlayersPredictions(week, season, F)
projs  <- calcPlayersProjections(scraps, yaml::read_yaml("./config/score_settings.yml"))

# PLAYERS AND MATCHUPS ####
# PLAYERS
source("./R/api/ffa_players.R")
players_stats <- ffa_players_stats(config$authToken, config$leagueId, season, 1) %>% 
  ffa_extractPlayersStats()

# MATCHUPS
source("./R/api/ffa_league.R")
leagueMatchups <- ffa_league_matchups(config$authToken, config$leagueId, week)
matchups_games <- ffa_extractMatchups(leagueMatchups)
teams_rosters  <- ffa_extractTeams(leagueMatchups)   

# REMAINING WORKFLOW ####

# carregando tabelas de "de para" de IDs de Jogadores
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
player_ids <- player_ids %>%
  mutate(
    id = as.integer(id), 
    nfl_id = as.integer(nfl_id)) %>%
  as_tibble()

# tipos de status que zera a pontuacao
injuryStatus <- c("Suspended","Injured Reserve","Not With Team")

# pega as projecoes e cruza com player stats para ver status de injury
projs %>% 
  inner_join(player_ids, by="id") %>% # unifica os ids
  inner_join(players_stats, by=c("nfl_id"="playerId")) %>% # adiciona info de status
  # colina duplicada vinda do Join
  select(-position.y) %>% 
  rename(position=position.x) %>% 
  ## Zera Estatisticas de quem estah machucado
  mutate(
    points  = ifelse(injuryGameStatus %in% injuryStatus, 0, points),
    floor   = ifelse(injuryGameStatus %in% injuryStatus, 0, floor),
    ceiling = ifelse(injuryGameStatus %in% injuryStatus, 0, ceiling),
    sd_pts  = ifelse(injuryGameStatus %in% injuryStatus, 0, sd_pts),
  )

# precisa adicioanr a informação de time
teams_rosters %>% 
  select(teamId, name, rosters, imageUrl) %>% 
  unnest(rosters)
  
  




# tipos de status que zera a pontuacao
injuryStatus <- c("Suspended","Injured Reserve","Not With Team")
# cola informacao de times as projecoes de pontos dos jogadores
projs.team <- projs %>% 
  inner_join(mutate(player_ids, id=as.integer(id)), by="id") %>% 
  addTeams(matchups, week, F) %>% 
  ## Zera Estatisticas de quem estah machucado
  mutate(
    points  = ifelse(injuryGameStatus %in% injuryStatus, 0, points),
    floor   = ifelse(injuryGameStatus %in% injuryStatus, 0, floor),
    ceiling = ifelse(injuryGameStatus %in% injuryStatus, 0, ceiling),
    sd_pts  = ifelse(injuryGameStatus %in% injuryStatus, 0, sd_pts),
  )




saveRDS(projs.team, glue("./data/week{week}_players_projections.rds"))
projs.team <- readRDS("./data/2019/week1_players_projections.rds")


##### import matchups
# source("./R/import/import_matchups.R")
# matchups <- importMatchups(week, .saveToFile = F)
# saveRDS(matchups, glue("./data/week{week}_matchups_json.rds"))
# source("./R/api/ffa_league.R")
# leagueMatchups <- ffa_league_matchups(config$authToken, config$leagueId, week)
# matchups_games <- ffa_extractMatchups(leagueMatchups)
# teams_rosters  <- ffa_extractTeams(leagueMatchups)   

##### import player statistics
# source("./R/import/import_player_stats.R")
# player_stats <- importPlayerStatistics(1:week, .saveToFile = F)
# saveRDS(player_stats, "./data/players_points.rds")
# source("./R/api/ffa_players.R")
# player_stats <- ffa_players_stats(config$authToken, config$leagueId, season, 1:week) %>% 
#   ffa_extractPlayersStats()

##### import predicions, calc projections and add Teams
# source("./R/import/ffa_player_projection.R")
# scraps <- scrapPlayersPredictions(week, season, F)
# projs  <- calcPlayersProjections(scraps, read_yaml("./config/score_settings.yml"))

# tipos de status que zera a pontuacao
# injuryStatus <- c("Suspended","Injured Reserve","Not With Team")

# cola informacao de times as projecoes de pontos dos jogadores
projs.team <- projs %>% 
  inner_join(mutate(player_ids, id=as.integer(id)), by="id") %>% 
  addTeams(matchups, week, F) %>% 
  ## Zera Estatisticas de quem estah machucado
  mutate(
    points  = ifelse(injuryGameStatus %in% injuryStatus, 0, points),
    floor   = ifelse(injuryGameStatus %in% injuryStatus, 0, floor),
    ceiling = ifelse(injuryGameStatus %in% injuryStatus, 0, ceiling),
    sd_pts  = ifelse(injuryGameStatus %in% injuryStatus, 0, sd_pts),
  )

## projection report
rmarkdown::render(
  input = "./R/reports/ffa_players_projection.Rmd",
  output_file = glue("../../{destPath}/reports/ffa_players_projection_week{week}.html"),
  output_format = "flex_dashboard",
  params = list(week=week)
)


# calcula tabela de pontuacao para todos os jogadores usa na simulacao
source("./R/simulation/players_projections.R")

# simula as partidas
source(glue("./R/simulation/points_simulation_v{sim.version}.R"))
sim <- simulateGames(week)

# constroi o relatório
rmarkdown::render(
  input = glue("./R/reports/dudes_simulation_v{sim.version}.Rmd"),
  output_file = glue("../../{destPath}/reports/dudes_simulation_week{week}_{prefix}_v{sim.version}.html"),
  output_format = "flex_dashboard",
    params = list(week=week)
  )

hist <- readRDS("./data/simulations_history.rds")
sim %>% 
  mutate(
    week = week,
    timestamp = now(),
    prefix = prefix
  ) %>% 
  bind_rows(hist) %>%
  saveRDS("./data/simulations_history.rds")


