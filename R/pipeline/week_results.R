library(tidyverse)
library(lubridate)
library(glue)
library(ffanalytics)

# parametros de execucao
weeks <- 1:13

# check Fantasy API
source("./R/import/checkFantasyAPI.R")
if(!checkFantasyAPI(1)) stop("Unable to access Fantasy API!")

# import machups
source("./R/import/import_matchups.R")
matchups <- weeks %>% 
  map( importMatchups, .saveToFile = F )

saveRDS(matchups, "./data/post_matchups_results.rds")

# import player statistics
source("./R/import/import_player_stats.R")
player_stats <- importPlayerStatistics(1:week, .saveToFile = F)
saveRDS(player_stats, "./data/players_points.rds")

