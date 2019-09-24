# lendo liga e token do yaml (para n?o versionar o access token)
library(yaml)
config <- yaml.load_file("./config/config.yml")
leagueId <- config$leagueId
authToken <- config$authToken


source("./R/api/ffa_league.R")
resp <- ffa_league_standings(.authToken = config$authToken, .week = 1, .leagueId = config$leagueId )

json <- resp$content

json$games$`102019`$leagues$`3940933`$teamIds
json$games$`102019`$leagues$`3940933`$teams[[1]] %>% flatten()


resp$response %>% 
  content("text") %>% 
  fromJSON(T)

