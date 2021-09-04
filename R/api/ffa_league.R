source("./R/api/ffa_api.R")

ffa_league_settings <- function(.authToken, .leagueId){
  ffa_api(
    .path = "/v2/league/settings",
    .query = list(
      "appKey"    = "internalemailuse",
      "leagueId"  = .leagueId
    ),
    .auth=.authToken)
}


# return the user id, leagues and teams of a 'authToken'
ffa_league_teams <- function(.authToken, .leagueId){
  
  players <- ffa_api(
    .path = "/v2/league/teams",
    .query = list(
      "appKey"    = "internalemailuse",
      "leagueId"  = .leagueId
    ),
    .auth=.authToken)
  
}


# return the user id, leagues and teams of a 'authToken'
ffa_league_matchups <- function(.authToken, .leagueId, .week){
  
  players <- ffa_api(
    .path = "/v2/league/matchups",
    .query = list(
      "appKey"    = "internalemailuse",
      "leagueId"  = .leagueId,
      "week"      =  .week,
      "includeRosters" = 1,
      "forcePlayoffs" = 1
    ),
    .auth=.authToken)
  
}

# return the user id, leagues and teams of a 'authToken'
ffa_league_standings <- function(.authToken, .leagueId, .week){
  
  players <- ffa_api(
    .path = "/v2/league/standings",
    .query = list(
      "appKey"    = "internalemailuse",
      "week"      = .week, 
      "leagueId"  = .leagueId
    ),
    .auth=.authToken)
  
}

# return the user id, leagues and teams of a 'authToken'
ffa_league_team_roster <- function(.authToken, .leagueId, .teamId, .week){
  
  players <- ffa_api(
    .path = "/v2/league/teams",
    .query = list(
      "appKey"    = "internalemailuse",
      "leagueId"  = .leagueId,
      "teamId"    = .teamId,
      "week"      = .week
    ),
    .auth=.authToken)
  
}

# return the user id, leagues and teams of a 'authToken'
ffa_league_matchups_recap <- function(.authToken, .leagueId, .week,.teamId){
  
  players <- ffa_api(
    .path = "/v2/league/team/matchuprecap",
    .query = list(
      "appKey"    = "internalemailuse",
      "leagueId"  = .leagueId,
      "week"      = .week,
      "teamId"    = .teamId
    ),
    .auth=.authToken)
  
}

ffa_extractRecap <- function(recapResp){
  if (length(recapResp$content)==0) return(NULL)
  tibble(
    team   = c("away","home"),
    teamId = recapResp$content$teams$id,
    name   = recapResp$content$teams$name,
    coachPoints = recapResp$content$teams$coach_points
  ) %>% 
    pivot_wider(names_from="team", values_from=c(teamId, name, coachPoints), names_sep=".") %>% 
    mutate(
      title = recapResp$content$title,
      week  = recapResp$content$week_num,
      paragraphs  = list(tibble(recapResp$content$paragraphs)), 
      leagueHighligths = list(tibble(recapResp$content$league_notes))
    ) %>% 
    return()
}

# extrai o time e o roster
ffa_extractTeams <- function(teamsResp){
  
  # extract teams
  teamsResp$content$games[[1]]$leagues[[1]]$teams %>% 
    #transforma a lista de times em tibble
    tibble(team=.) %>% 
    unnest_wider(team) %>% 
    # corrige tipos inteiros
    mutate(across(c(teamId, ownerUserId), as.integer)) %>% 
    return()
}



# extrai o time e o roster
ffa_extractTeamsFromMatchups <- function(leagueMatchupsResp){
  
  # extract teams and rosters
  leagueMatchupsResp$content$games[[1]]$leagues[[1]]$teams %>% 
    #transforma a lista de times em tibble
    tibble(team=.) %>% 
    unnest_wider(team) %>% 
    # corrige tipos inteiros
    mutate(across(c(teamId, ownerUserId), as.integer)) %>% 
    # transforma a lista de rosters (em cada time) em um tibble
    mutate( rosters = map(rosters, function(r){
      r[[1]] %>% 
        bind_rows(.id="slotPosition") %>% 
        as_tibble() %>% 
        mutate(across(rosterSlotId:playerId,as.integer)) %>% 
        return()
    })) %>% 
    # transform as estatisticas semanais em tibble
    mutate( week.stats = map(stats, function(.stat){
      .stat$week$`2021` %>%
        tibble(week=names(.), week.stats=.) %>%
        unnest_wider(week.stats) %>% 
        mutate( week = as.integer(week) ) %>% 
        mutate( pts  = ifelse("pts" %in% names(.), as.numeric(pts), as.numeric(0)) ) %>% 
        return()
    })) %>% 
    # transforma as estatisticas da temporada em tibble
    mutate( season.stats = map(stats, function(.stat){
      .stat$season %>% tibble(season.stats=.) %>% 
        unnest_wider(season.stats)    
    }))

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
