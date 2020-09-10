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
