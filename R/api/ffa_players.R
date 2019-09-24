source("./R/api/ffa_api.R")

# return the user id, leagues and teams of a 'authToken'
ffa_players_all <- function(.authToken, .leagueId){

  players <- ffa_api2(
    .path = "v2/league/players",
    .query = list(
      "appKey"    = "internalemailuse",
      "authToken" = .authToken,
      "leagueId"  = .leagueId,
      "count"     = 2000
    ))
  
}

ffa_players_resp_to_tibble <- function(.resp) {
  .resp$content$games$`102019`$players %>%
    map_dfr(function(.player){
      .player %>% 
        map(~ ifelse(is.null(.x), NA, .x)) %>% 
        as_tibble() %>% 
        return()
    }) %>% 
    mutate_at(vars(playerId, nflTeamId, byeWeek), as.integer) %>% 
    mutate_at(vars(esbId, position, nflTeamAbbr, injuryGameStatus), factor) %>% 
    return()
}
