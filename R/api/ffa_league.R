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

# return the user id, leagues and teams of a 'authToken'
ffa_league_team_roster <- function(.authToken, .leagueId, .teamId, .week){
  
  players <- ffa_api(
    .path = "/v2/league/teams",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "leagueId"  = .leagueId,
      "teamId"    = .teamId,
      "week"      = .week
    ))
  
}

# extrai o time e o roster
ffa_extractTeams <- function(leagueMatchupsResp){
  
  # extract teams and rosters
  leagueMatchupsResp$content$games[[1]]$leagues[[1]]$teams %>% 
    tibble(team=.) %>% 
    unnest_wider(team) %>% 
    mutate(across(c(teamId, ownerUserId), as.integer)) %>% 
    select(-matchups, -stats) %>% 
    mutate( rosters = map(rosters, function(r){
      r[[1]] %>% 
        bind_rows(.id="slotPosition") %>% 
        as_tibble() %>% 
        mutate(across(rosterSlotId:playerId,as.integer)) %>% 
        return()
    })) %>% 
    return()
  
}

# extrai os jogos
ffa_extractMatchups <- function(leagueMatchupsResp){
  
  # extract matchups
  leagueMatchupsResp$content$games[[1]]$leagues[[1]]$matchups %>% 
    tibble(matchups=.) %>% 
    unnest_wider(matchups) %>% 
    unnest_wider(awayTeam, names_sep=".") %>% 
    unnest_wider(homeTeam, names_sep=".") %>% 
    mutate(across(c(week, ends_with("teamId")), as.integer)) %>% 
    return()
  
}





