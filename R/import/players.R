library(yaml)
source("./R/api/ffa_players.R")

config <- yaml::read_yaml("./config/config.yml")

players <- ffa_players_all(.authToken = config$authToken, .leagueId = config$leagueId)


ffa_players_resp_to_tibble(players) %>% 
  saveRDS("./data/players.rds")
