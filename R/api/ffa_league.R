source("./R/api/ffa_api.R")

ffa_league_settings <- function(.authToken, .leagueId){
  ffa_api(
    .path = "/v2/league/settings",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "leagueId"  = .leagueId
    ))
}


# return the user id, leagues and teams of a 'authToken'
ffa_league_teams <- function(.authToken, .leagueId){
  
  players <- ffa_api(
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
    .path = "/v2/league/matchups",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "leagueId"  = .leagueId,
      "week"      =  .week,
      "includeRosters" = 1
    ))
  
}

# return the user id, leagues and teams of a 'authToken'
ffa_league_standings <- function(.authToken, .leagueId, .week){
  
  players <- ffa_api(
    .path = "/v2/league/standings",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "week"      = .week, 
      "leagueId"  = .leagueId
    ))
  
}







