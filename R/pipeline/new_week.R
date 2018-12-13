library(knitr)
library(markdown)
library(flexdashboard)
library(lubridate)
library(glue)
source("./R/import/checkFantasyAPI.R")
source("./R/import/import_matchups.R")
source("./R/simulation/points_simulation_v3.R")

week <- 15
prefix <- "preFA"

checkFantasyAPI(week)

# import matchups
importMatchups(week) -> matchups

# import player statistics
source("./R/import/import_player_stats.R")

# import predicions, calc projections and add Teams
source("./R/import/ffa_player_projection.R")
scraps <- scrapPlayersPredictions(week)
projs  <- calcPlayersProjections(scraps)
projs.team <- addTeams(projs, matchups, week)

## projection report
rmarkdown::render(
  input = "./R/reports/ffa_players_projection.Rmd",
  output_file = glue("../../public/ffa_players_projection_week{week}.html"),
  output_format = "flex_dashboard",
  params = list(week=week)
)


# calcula tabela de pontuacao para todos os jogadores usa na simulacao
source("./R/simulation/players_projections.R")

# simula as partidas
simulateGames(week) -> sim

# constroi o relatório
rmarkdown::render(
    input = "./R/reports/dudes_simulation_v2.Rmd",
    output_file = glue("../../public/dudes_simulation_week{week}_{prefix}.html"),
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


