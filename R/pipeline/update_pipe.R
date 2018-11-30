library(knitr)
library(markdown)
library(flexdashboard)
library(lubridate)
library(glue)
source("./R/import/checkFantasyAPI.R")
source("./R/import/import_matchups.R")
source("./R/simulation/points_simulation_v3.R")

week <- 13
prefix <- "posTNF"

checkFantasyAPI(week)

importMatchups(week) -> json

simulateGames(week) -> sim


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
