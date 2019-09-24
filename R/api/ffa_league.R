source("./R/api/ffa_api.R")

# return the user id, leagues and teams of a 'authToken'
# ffa_league_matchups <- function(.authToken, .leagueId, .week){
#   ffa_api(
#     .path = "v1/league/matchups",
#     .query = list(
#         "authToken" = .authToken,
#         "leagueId"  = .leagueId,
#         "week"      = .week
#       ))
# }

ffa_league_settings <- function(.authToken, .leagueId){
  ffa_api2(
    .path = "/v2/league/settings",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "leagueId"  = .leagueId
    ))
}


# return the user id, leagues and teams of a 'authToken'
ffa_league_teams <- function(.authToken, .leagueId){
  
  players <- ffa_api2(
    .path = "/v2/league/teams",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "leagueId"  = .leagueId
    ))
  
}


# return the user id, leagues and teams of a 'authToken'
ffa_league_matchups <- function(.authToken, .leagueId, .week){
  
  players <- ffa_api(
    .path = "/league/matchups",
    .query = list(
       #"appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "leagueId"  = .leagueId,
      "week"      =  .week
    ))
  
}

# return the user id, leagues and teams of a 'authToken'
ffa_league_standings <- function(.authToken, .leagueId, .week){
  
  players <- ffa_api2(
    .path = "/v2/league/standings",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "week"      = .week, 
      "leagueId"  = .leagueId
    ))
  
}

