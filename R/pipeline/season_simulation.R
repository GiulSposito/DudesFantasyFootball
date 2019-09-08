library(knitr)
library(markdown)
library(flexdashboard)
library(lubridate)
library(glue)
library(ffanalytics)

#### load PLAYERS_ID from FFANALYTICS package ####
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
player_ids <- player_ids %>% 
  mutate(id=as.integer(id),
         nfl_id=as.integer(nfl_id)) %>% 
  as_tibble()


#### Check FANTASY API ####
source("./R/import/checkFantasyAPI.R")
if (!checkFantasyAPI(1)) stop("FANTASY API FAILED")

#### Season Games ####
source("./R/import/import_matchups.R")
seasonWeeks <- 1:14
seasonGames <- map(seasonWeeks,importMatchups, saveToFile=F)
saveRDS(seasonGames, "./data/draft/seasonGames.rds")


# do season scrapping
source("./R/import/ffa_player_projection.R")
seasonScraps < map(seasonWeeks,scrapPlayersPredictions, saveToFile=F,
                          .progress=T,
                          .options = future_options(packages=c("ffanalytics")))


# current pipeline
# 
# source("./R/simulation/points_simulation_v3.R")
# week <- 1
# prefix <- "preTNF"
# 
# # import player statistics
# source("./R/import/import_player_stats.R")
# 
# # import predicions, calc projections and add Teams
# source("./R/import/ffa_player_projection.R")
# scraps <- scrapPlayersPredictions(week)
# projs  <- calcPlayersProjections(scraps)