library(tidyverse)
library(lubridate)
library(glue)
library(ffanalytics)
library(flexdashboard)

# parametros de execucao
week <- 16
sim.version <- 4
prefix <- "posWaiversAndTrades"
destPath <- "static"

# check Fantasy API
source("./R/import/checkFantasyAPI.R")
if(!checkFantasyAPI(week)) stop("Unable to access Fantasy API!")

# carregando tabelas de "de para" correcao do ID de jogadores
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
player_ids <- player_ids %>% 
  mutate( 
    id     = as.integer(id), 
    nfl_id = as.integer(nfl_id)) %>%
  # joey slye fixies
  mutate(
    nfl_id        = ifelse(id==14600, 2563132,     nfl_id),
    fantasypro_id = ifelse(id==14600, "joey-slye", fantasypro_id),
    fftoday_id    = ifelse(id==14600, "16763",     fftoday_id),
  ) %>%  
  as_tibble()


# import matchups
source("./R/import/import_matchups.R")
matchups <- importMatchups(week, .saveToFile = F)
saveRDS(matchups, glue("./data/week{week}_matchups_json.rds"))


# import player statistics
source("./R/import/import_player_stats.R")
player_stats <- importPlayerStatistics(1:week, .saveToFile = F)
saveRDS(player_stats, "./data/players_points.rds")

# import predicions, calc projections and add Teams
source("./R/import/ffa_player_projection.R")
scraps <- scrapPlayersPredictions(week)
projs  <- calcPlayersProjections(scraps)

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


