library(tidyverse)
library(lubridate)
library(flexdashboard)
library(glue)
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!

player_ids <- player_ids %>% 
  mutate(id=as.integer(id),
         nfl_id=as.integer(nfl_id)) %>% 
  as_tibble()


source("./R/import/checkFantasyAPI.R")
source("./R/import/import_matchups.R")
source("./R/simulation/points_simulation_v3.R")

week <- 1
prefix <- "PreMNF"

checkFantasyAPI(week)

importMatchups(week) -> matchups

# # import predicions, calc projections and add Teams
# source("./R/import/ffa_player_projection.R")
# rmarkdown::render(
#   input = "./R/reports/ffa_players_projection.Rmd",
#   output_file = glue("../../public/ffa_players_projection_week{week}.html"),
#   output_format = "flex_dashboard",
#   params = list(week=week)
# )

sim <- simulateGames(week)

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


