library(tidyverse)
library(lubridate)
library(glue)
library(ffanalytics)

# parametros de execucao
weeks <- 1:10

# check Fantasy API
source("./R/import/checkFantasyAPI.R")
if(!checkFantasyAPI(1)) stop("Unable to access Fantasy API!")

# import machups
source("./R/import/import_matchups.R")
matchups <- weeks %>% 
  map(
    importMatchups,
    .saveToFile = F
  )

saveRDS(matchups, "./data/post_matchups_results.rds")