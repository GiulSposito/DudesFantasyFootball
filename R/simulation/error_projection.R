library(tidyverse)

# funcao que considera os erros das projecoes passadas na projecao atual
# gera uma nova tabela de projecao com pontos gerados a partir do erro de cada data_src
# para cada player_id nas semanas anteriores aplicado nas projecoes da semana atual
applyProjectionErrors <- function(.curr.week, .points.projection, .players.points){
  
  # original datasource
  # curr.week <- 5
  # points.projection <- readRDS("./data/points_projection.rds")
  # players.points <- readRDS("./data/players_points.rds")

  # gera a tabela de distribuicao de erros passados
  # para cada semana, jogador e data_src, calcula o erro da projecao
  error.dist <- .players.points %>% 
    select(week, id, points) %>% 
    inner_join(.points.projection, by = c("week", "id")) %>% 
    mutate( error = points - pts.proj ) %>% 
    filter( week < .curr.week ) %>% 
    select( id, data_src, error )
  
  # entao aplica a tabela de erros para cada jogador e datasorce
  # na projecao da semana atual, gerando uma nova colecao de pontos
  # por data_src
  pts.proj.errors <-  .points.projection %>% 
    filter(week==.curr.week) %>% 
    inner_join( error.dist, by = c("data_src", "id")) %>% 
    filter( pts.proj != 0 ) %>%  # não projeta desvio se a projecao for zero
    mutate( error.proj = error + pts.proj ) %>% 
    select(week, pos, data_src, id, pts.proj=error.proj, season) %>% 
    mutate( type = "error.dist")
  
  # entao junta a nova tabela de pontos projetados com base nos erros de previsao
  # com a tabela atual de projecao e devolve
  .points.projection %>% 
    filter(week==.curr.week) %>% 
    mutate( type = "site.proj" ) %>% 
    bind_rows(pts.proj.errors) %>% 
    mutate( type=factor(type) ) %>% 
    arrange(id, data_src, type) %>% 
    return()

}

.testErrorProj <- function(){
  applyProjectionErrors(
    5,
    readRDS("./data/points_projection.rds"),
    readRDS("./data/players_points.rds")
  ) %>% 
    ggplot(aes(x=pts.proj, color=type, fill=type)) +
    geom_density(alpha=.5) +
    facet_grid(pos~data_src, scales = "free") +
    theme_minimal()
}





