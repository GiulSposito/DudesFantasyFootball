library(tidyverse)

# mapping src_id (nfl) <-> id (ffa)
.players_id <- readRDS("./data/nfl_players_id.rds")

# translatig NFL ID to FFA ID
nfl2ffa <- function(.dtf, .ids=.players_id) {
  .ids %>% 
    select(id, src_id) %>% 
    right_join(.dtf, by="src_id") %>% 
    select(-src_id) %>% 
    return()
}

# funcao para extrar dados dos hosters dos times
.extractTeamRoster <- . %>% 
  .$players %>% 
  .[[1]] %>% 
  select( src_id=id, name, position, rosterSlot, fantasyPts ) %>%
  jsonlite::flatten() %>% 
  select(-fantasyPts.week.season, -fantasyPts.week.week ) %>% 
  rename(points = fantasyPts.week.pts) %>% 
  mutate(
    src_id = as.integer(src_id),
    points = as.numeric(points)
  )

# precessa o json de retorno

extractTeams <- function(.matchup.teams.json) {
  .matchup.teams.json %>% 
    map(function(.json){
      matchup <- .json$leagues$matchup
      tibble(
        home.teamId = matchup$homeTeam$id,
        home.name   = matchup$homeTeam$name,
        home.logo   = matchup$homeTeam$logoUrl,
        home.pts    = as.numeric(matchup$homeTeam$pts),
        home.roster = list(.extractTeamRoster(matchup$homeTeam)),
        away.teamId = matchup$awayTeam$id,
        away.name   = matchup$awayTeam$name,
        away.logo   = matchup$awayTeam$logoUrl,
        away.pts    = as.numeric(matchup$awayTeam$pts),
        away.roster = list(.extractTeamRoster(matchup$awayTeam))
      ) %>% 
        return()
    }) %>% bind_rows() %>% 
    return()
}

