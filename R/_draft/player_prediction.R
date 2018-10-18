library(ffanalytics)
source("./R/_draft/score_settings.R")

weeks <- 1:6

weeks %>% 
  map( function(.week){
    scrape_data(pos = c("QB", "RB", "WR", "TE", "DST", "K"),season = 2018, week = .week)
  }) -> scraps
  
scraps %>% 
  saveRDS("./data/weeklies_scraps.rds")


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
  source_points(.scrap, .score.settings) %>% 
    filter( pos!="K") -> players.projection 

  # pega a pontuacao dos kickers do scrap
  # e junta com a projecao
  .scrap$K %>%
    mutate(pos="K") %>% 
    select(pos, data_src, id, points=site_pts) %>% 
    bind_rows(players.projection) %>% 
    return()
  
}

scraps %>% 
  map(playerPointsProjections,
      .score.settings = dudes.score.settings) %>% 
  bind_rows(.id="week") %>% 
  mutate( season=2018,
          week = as.integer(week),
          id = as.integer(id) ) -> points.projection


# extrai o mapeamento ID do Fantasy versus ID da projecao
scraps[[1]] %>%
  map(function(dft){
    dft %>% 
      filter(data_src=="NFL") %>% 
      select(id, src_id, player, team, pos) %>% 
      return()
  }) %>% 
  bind_rows() %>%
  distinct() %>% 
  mutate_at(vars(id, src_id), as.integer) -> players.ids

players.ids %>% 
  inner_join(points.projection, by = c("id")) -> players.projection

players.projection %>% 
  saveRDS("./data/players_projections.rds")
  
