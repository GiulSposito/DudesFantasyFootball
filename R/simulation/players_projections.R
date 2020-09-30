# funcao que aproveita o pacote ffanalytics para fazer a projecao de pontos por jogador
playerPointsProjections <- function(.scrap, .score.settings){
  
  source("../ffanalytics/R/calc_projections.R")
  source("../ffanalytics/R/stats_aggregation.R")
  source("../ffanalytics/R/source_classes.R")
  source("../ffanalytics/R/custom_scoring.R")
  source("../ffanalytics/R/scoring_rules.R")
  source("../ffanalytics/R/make_scoring.R")
  source("../ffanalytics/R/recode_vars.R")
  source("../ffanalytics/R/impute_funcs.R")
  
  # calcula 
  players.projection  <- source_points(.scrap, .score.settings)
  return(players.projection)
  
}

calcPointsProjection <- function(.season, .score.settings, saveToFile=T){
  
  points.projection <- readRDS("./data/weeklies_scraps.rds") %>% 
    map(playerPointsProjections,
        .score.settings = .score.settings) %>% 
    bind_rows(.id="week") %>% 
    mutate( season=.season,
            week = as.integer(week),
            id = as.integer(id) ) %>% 
    rename(pts.proj = points) %T>% 
    # salva pontuacao projetada
    saveRDS("./data/points_projection.rds") %>% 
    return()
  
}

# funcao que calcula o erro de projecao dos sites por jogador 
projectErrorPoints <- function(.players_stats, .ptsproj, .my_player_ids, .week){
  
  # .players_stats = players_stats
  # .ptsproj = ptsproj
  # .my_player_ids = my_player_ids
  # .week = 2
  
  error_table <- .players_stats %>% 
    unnest(weekPts) %>% 
    select(playerId, name, week, weekPts) %>%
    filter(week<.week) %>% 
    inner_join(select(.my_player_ids, id, playerId=nfl_id), by="playerId") %>% 
    filter(!is.na(weekPts)) %>% 
    inner_join(.ptsproj, by = c("week", "id")) %>% 
    filter(week<.week) %>% 
    select(playerId, id, data_src, name, pos, week, weekPts, pts.proj) %>% 
    mutate(proj.error = weekPts-pts.proj,
           lag = .week-week) %>% 
    select(playerId, id, data_src, proj.error, lag)
  
  .ptsproj %>% 
    filter(week==.week) %>% 
    inner_join(error_table, by = c("data_src", "id")) %>% 
    mutate( new.pts.projs = pts.proj + proj.error) %>% 
    mutate( data_src = paste0(data_src,"_ERROR_LAG_", lag) ) %>% 
    select(week, pos, data_src, id, pts.proj=new.pts.projs, season) %>% 
    return()
}

projectFloorCeiling <- function(.proj_table, .week, .season){
  .proj_table %>% 
    select(id, floor, ceiling) %>% 
    pivot_longer(cols = c(-id), values_to = "pts.proj", names_to="data_src") %>% 
    mutate(week=.week, season=.season) %>% 
    inner_join(select(.proj_table, id, pos), by="id") %>% 
    select(week, pos, data_src, id, pts.proj, season) %>% 
    return()
}

