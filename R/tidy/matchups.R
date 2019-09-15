library(tidyverse)

# mapping src_id (nfl) <-> id (ffa)
.players_id <- player_ids # readRDS("./data/nfl_players_id.rds")

# translatig NFL ID to FFA ID
nfl2ffa <- function(.dtf, .ids=.players_id) {
  .ids %>% 
    mutate(src_id=as.integer(nfl_id)) %>% 
    right_join(select(.dtf,-id), by="src_id") %>% 
    select(-src_id) %>% 
    return()
}

# precessa o json de retorno

extractTeams <- function(.matchup.teams.json) {
  
  flatTeamRoster <- function(.teamJson){
    .teamJson %>% 
      jsonlite::flatten() %>% 
      as_tibble() %>% 
      select(-videos) %>% 
      mutate_at(vars(id, fantasyPts.week.season, fantasyPts.week.week, 
                     fantasyProjectedPts.week.season, fantasyProjectedPts.week.week), as.integer) %>% 
      mutate_at(vars(fantasyPts.week.pts, fantasyProjectedPts.week.pts), as.numeric) %>% 
      select(-fantasyPts.week.season, -fantasyPts.week.week ) %>% 
      rename(points = fantasyPts.week.pts) %>% 
      mutate(
        src_id = as.integer(id),
        points = as.numeric(points)
      ) %>% 
      return()
  } 
  
  .matchup.teams.json %>% 
    map(function(.json){
      .json$leagues$matchup %>% 
        jsonlite::flatten() %>% 
        set_names(gsub("Team\\.", "\\.", names(.))) %>%
        as_tibble() %>% 
        mutate_at(vars(week, away.id, home.id), as.integer) %>% 
        mutate_at(vars(away.pts, home.pts), as.numeric) %>% 
        mutate( away.players = map(away.players, flatTeamRoster),
                home.players = map(home.players, flatTeamRoster) ) %>%
        return()
    }) %>%
    bind_rows() %>% 
    return()
}

