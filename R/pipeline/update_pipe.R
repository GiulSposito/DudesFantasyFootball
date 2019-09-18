library(tidyverse)
library(lubridate)
library(flexdashboard)
library(glue)


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


source("./R/import/checkFantasyAPI.R")
source("./R/import/import_matchups.R")
source("./R/simulation/points_simulation_v3.R")

week <- 3
prefix <- "PreTNF"

checkFantasyAPI(week)

importMatchups(week) -> matchups

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


