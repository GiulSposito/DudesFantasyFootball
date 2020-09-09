# library(tidyverse)
# library(lubridate)
# library(glue)
# library(ffanalytics)
# library(flexdashboard)
library(yaml)

# EXECUTION PARAMETERS ####
week <- 1
season <- 2020
config <- read_yaml("./config/config.yml")
prefix <- "preTNF"
destPath <- "../../static/reports/2020"
sim.version <- 3

# API ACCESS CHECK ####
source("./R/import/checkFantasyAPI.R")
if(!checkFantasyAPI(config$authToken, config$leagueId, week)) stop("Unable to access Fantasy API!")

# SCRAPPING AND PROJECTION ####
source("./R/import/ffa_player_projection.R")
scraps <- scrapPlayersPredictions(week, season, T)
projs  <- calcPlayersProjections(scraps, read_yaml("./config/score_settings.yml"))

# PLAYERS AND MATCHUPS ####
# PLAYERS
source("./R/api/ffa_players.R")
players_stats <- ffa_players_stats(config$authToken, config$leagueId, season, 1:week) %>% 
  ffa_extractPlayersStats()

# MATCHUPS
source("./R/api/ffa_league.R")
leagueMatchups <- ffa_league_matchups(config$authToken, config$leagueId, week)
matchups_games <- ffa_extractMatchups(leagueMatchups)
teams_rosters  <- ffa_extractTeams(leagueMatchups)   

# TEST BRANCH: TEAM ROSTERS ####
# enquanto não vem a informação do roster, simula a alocação baseada
# numa alocação de jogador x time da temporada passada

old_teams <- readRDS(glue("./data/2019/week{week}_players_projections.rds")) %>% 
  select(id, fantasy.team, playerId=nfl_id) %>% 
  filter(fantasy.team!="*FreeAgent") %>% 
  mutate(
    teamId = case_when(
      fantasy.team=="Boys" ~ 8,
      fantasy.team=="Steelers" ~ 5,
      fantasy.team=="Pfeiferians" ~ 6,
      fantasy.team=="Robots" ~ 1,
      fantasy.team=="Knights" ~ 7,
      fantasy.team=="Mules" ~ 2,
      fantasy.team=="Giants" ~ 11,
      fantasy.team=="Blues" ~ 9,
      fantasy.team=="Bikers" ~ 4,
      fantasy.team=="Riders" ~ 3,
    )
  ) %>% 
  arrange(teamId)

# precisa adicioanr a informação de time
# team_allocation <- teams_rosters %>% 
#   select(teamId, name, rosters) %>% 
#   unnest(rosters) %>% 
#   mutate(fantasy.team = str_remove(name, "(.+ )?")) %>% 
#   select(teamId, fantasy.team)

team_allocation <- old_teams

# TABELA DE PROJECAO ####

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
players_projs <- projs %>% 
  inner_join(player_ids, by="id") %>% # unifica os ids
  inner_join(players_stats, by=c("nfl_id"="playerId")) %>% # adiciona info de status
  # colina duplicada vinda do Join
  select(-position.y) %>% 
  rename(position=position.x) %>% 
  # pega somente a semana de interesse
  unnest(weekPts) %>% 
  filter(week==week) %>% 
  ## Zera Estatisticas de quem estah machucado
  mutate(
    points  = ifelse(injuryGameStatus %in% injuryStatus, 0, points),
    floor   = ifelse(injuryGameStatus %in% injuryStatus, 0, floor),
    ceiling = ifelse(injuryGameStatus %in% injuryStatus, 0, ceiling),
    sd_pts  = ifelse(injuryGameStatus %in% injuryStatus, 0, sd_pts),
  ) %>% 
  # adiciona a informacao do time "owner"
  left_join(team_allocation, by=c("nfl_id"="playerId")) %>% 
  # quem nao tem time vira "Free Agent"
  mutate(fantasy.team=if_else(is.na(fantasy.team),"*FreeAgent", fantasy.team))


# salva estatisticas dos jogadores
players_stats %>% 
  unnest(weekPts) %>% 
  inner_join(player_ids, by=c("playerId"="nfl_id")) %>% 
  mutate(nfl_id=playerId) %>%
  saveRDS("./data/players_points.rds")

# salva projecoes  
saveRDS(players_projs, glue("./data/week{week}_players_projections.rds"))

# deveria salvar?
# matchups_games
# teams_rosters


# PROJECTION REPORT ####
rmarkdown::render(
  input = "./R/reports/ffa_players_projection.Rmd",
  output_file = glue("../../{destPath}/reports/ffa_players_projection_week{week}.html"),
  output_format = "flex_dashboard",
  params = list(week=week)
)

# SIMULACAO ####

# calcula tabela de pontuacao para todos os jogadores usa na simulacao
source("./R/simulation/players_projections.R")
ptsproj <- calcPointsProjection(season, read_yaml("./config/score_settings.yml"))

# simula as partidas
source(glue("./R/simulation/points_simulation_v{sim.version}.R"))
sim <- simulateGames(week)


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
# projs.team <- projs %>% 
#   inner_join(mutate(player_ids, id=as.integer(id)), by="id") %>% 
#   addTeams(matchups, week, F) %>% 
#   ## Zera Estatisticas de quem estah machucado
#   mutate(
#     points  = ifelse(injuryGameStatus %in% injuryStatus, 0, points),
#     floor   = ifelse(injuryGameStatus %in% injuryStatus, 0, floor),
#     ceiling = ifelse(injuryGameStatus %in% injuryStatus, 0, ceiling),
#     sd_pts  = ifelse(injuryGameStatus %in% injuryStatus, 0, sd_pts),
#   )

## projection report
# rmarkdown::render(
#   input = "./R/reports/ffa_players_projection.Rmd",
#   output_file = glue("../../{destPath}/reports/ffa_players_projection_week{week}.html"),
#   output_format = "flex_dashboard",
#   params = list(week=week)
# )


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


