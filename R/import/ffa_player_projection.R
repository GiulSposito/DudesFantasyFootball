library(ffanalytics)
library(glue)

scrapPlayersPredictions <- function(.week, .season) {
  # # faz o scraping de projeção dos sites
  
  # quais sites (default)
  #  
  # 
  sources <- c("ESPN", "FantasyData", "FantasySharks", "FFToday",
               "FleaFlicker", "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL", 
               "RTSports")
  
  # "CBS", "FantasyPros", 
  
  scrap <- scrape_data(
    src = sources,
    pos = c("QB", "RB", "WR", "TE", "K", "DST"),
    season = .season,
    week = .week
  )
  
  return(scrap)
}

saveScraps <- function(.week, .scrps){
# salva arquivos temporariamente
  .scrps %>% saveRDS(glue("./data/week{.week}_scrap.rds"))
  
  # adiciona scrap da semana no banco de projecoes
  if(file.exists("./data/weeklies_scraps.rds")) {
    wscraps <- readRDS("./data/weeklies_scraps.rds")
  } else {
    wscraps <- list()
  }
  wscraps[[.week]] <- .scrps
  saveRDS(wscraps,"./data/weeklies_scraps.rds")
}

# faz a tabela de projeção de resultados
calcPlayersProjections <- function(.week_scrap, .scoring_rules) {
  players.proj <- projections_table(.week_scrap, .scoring_rules) 
  
  players.proj %>% 
    filter(avg_type=="average") %>%
    add_player_info() %>% 
    mutate(id=as.integer(id)) %>% 
    arrange(id) %>%
    filter( position %in% c("QB", "RB", "WR", "TE", "K", "DST")) %>% 
    return()
}


addTeams <- function(.projections, .weekMatchups, .week, .saveFile=T){
  # matchups and rosters (nfl)
  source("./R/tidy/matchups.R")
  teams <- .weekMatchups %>% 
    extractTeams() %>% 
    mutate( 
      home.roster = map(home.players, nfl2ffa, .ids=.players_id), # to ffa ids
      away.roster = map(away.players, nfl2ffa, .ids=.players_id)  # to ffa ids
    )
  
  # tydi teams
  c("home","away") %>% 
    map(
      function(.prefix,.teams) {
        .teams %>% 
          select(starts_with(.prefix)) %>% 
          set_names(gsub(pattern=paste0(.prefix,"\\."),replacement = "",x=names(.))) %>%
          select(-pts) %>%
          rename(teamName=name) %>% 
          return()
      },
      .teams=teams
    ) %>% 
    bind_rows() %>% 
    unnest(roster) %>% 
    select(id=id1, fantasy.team = teamName, injuryGameStatus) -> players.team
  
  players.team %>%
    mutate(id=as.integer(id)) %>% 
    right_join(.projections, by = "id") %>%
    mutate(
      fantasy.team = gsub("([a-zA-Z\']+ )?", "", fantasy.team),
      fantasy.team = case_when(is.na(fantasy.team) ~ "*FreeAgent",
                               TRUE ~ fantasy.team)
    ) -> week.projections
  
  if (.saveFile) saveRDS(week.projections, glue("./data/week{.week}_players_projections.rds"))
  return(week.projections)
}

