library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(ffanalytics)

scrapNflFantasyProjection <- function(.authToken, .leagueId, .season=2020, .week) {
  
  # test parameters
  # .config    = yaml::read_yaml("./config/config.yml")
  # .authToken = .config$authToken
  # .leagueId  = .config$teamId
  # .season    = 2020
  # .week     = 1
  
  
  ## Fantasy Projections #####
  
  # Passos:
  #  1) Consulta na API os IDs das Estatisticas de Projecao
  #  2) Consulta as Estatisticas de Projecao
  #  3) Consulta a lista de jogadores para pegar dados de id, posicao, nome e time
  #  4) Cria uma LONG table FANTASY_STAT_ID x STATS
  #  5) Pega a definicação de scraping do FFANALYTICS para a NFL e cria uma table STAT_ID x FFA SCRAP COL NAME
  #  6) Finalmente cruza STATS x PLAYERS x FFA SCRAP COL NAMES
  
  ## Statistics IDs
  
  resp <- GET("https://api.fantasy.nfl.com/v2/game/stats?appKey=internalemailuse") %>%
    content(as="text")
  
  json_stat_id <- resp %>%
    jsonlite::fromJSON(flatten = T, simplifyDataFrame = T)
  
  stat_ids <- json_stat_id$games$`102020`$stats %>%
    map_df(function(.stat){
      .stat %>%
        keep(~!is.null(.x)) %>%
        as_tibble()
    })
  
  ## Week Projections
  
  resp <- GET(glue("https://api.fantasy.nfl.com/v2/players/weekprojectedstats?season={.season}&week={.week}")) %>%
    content(as="text")
  
  json_stats <- resp %>%
    fromJSON(flatten = T, simplifyDataFrame = T)
  
  stats <- json_stats$games$`102020`$players %>%
    tibble(id=names(.),
           values=.) %>%
    mutate(values = map(values, function(.x){
      # id$projectedStats$week$2020$1$stats
      .x[[1]][[1]][[1]][[1]] %>%
        as_tibble()
    })) %>%
    unnest(values)
  
  ## join stats ID with Projections
  
  fantasyProj <- stats %>%
    pivot_longer(cols=c(-id), names_to="stat_id") %>%
    rename(playerId=id) %>%
    left_join(stat_ids, by=c("stat_id"="id"))
  
  # PLAYERS
  source("./R/api/ffa_players.R")
  players <- ffa_players(config$authToken, config$leagueId) %>% 
    ffa_extractPlayers()
  
  nflPlayers <- players %>% 
    select(src_id=playerId, player=name, position, team=nflTeamAbbr) %>% 
    mutate(
      src_id=as.character(src_id),
      team=if_else(team=="",as.character(NA), team)
    )
  
  ## NFL Proj Col Especification x Fan
  ffa_columns = tibble(
    colName = names(projection_sources[["NFL"]]$stat_cols),
    shortName = projection_sources[["NFL"]]$stat_cols
  )
  
  ## IDS do FFA
  # carregando tabelas de "de para" de IDs de Jogadores
  load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
  miss_ids <- readRDS("./data/playerIds_not_mapped.rds") %>% 
    mutate( 
      id = as.character(id), 
      nfl_id = as.character(nfl_id)
    )
  
  ffa_player_ids <- player_ids %>%
    # completa a tabela de mapeamento de projecoes do ffanalytics
    bind_rows(miss_ids) %>% 
    as_tibble()
  
  
  ## gerando uma tabela de scraping igual da FFA
  nfl_scrap_table <- fantasyProj %>%
    inner_join(ffa_columns, by = "shortName") %>%
    mutate(value=as.numeric(value)) %>%
    select(src_id=playerId, colName, value) %>%
    filter(complete.cases(.)) %>%
    distinct() %>%
    pivot_wider(id_cols=src_id, names_from=colName, values_from=value) %>% 
    inner_join(select(ffa_player_ids, id, nfl_id), by=c("src_id"="nfl_id")) %>% 
    inner_join(nflPlayers, by="src_id") %>% 
    mutate(
      data_src="NFL",
      week=as.character(.week),
      position = if_else(position=="DEF","DST",position)
    ) %>% 
    select(data_src, id, src_id, player, position, team, everything())
  
  # retorna tabela
  return(nfl_scrap_table)
}

# funcao que adiciona uma tabela de scraping à lista no FFA
addScrapTable <- function(.scrp_data, .scrp_table){
  # para cada posicao dos scraping
  
  names(.scrp_data) %>% 
    map(function(pos, .scrp_dt, .scrp_tb){
      pos_col <- intersect(names(.scrp_dt[[pos]]),names(.scrp_tb))
      .scrp_tb %>% 
        filter(position==pos) %>% 
        select(all_of(pos_col)) %>% 
        bind_rows(.scrp_dt[[pos]]) %>% 
        distinct()
    },
    .scrp_dt = .scrp_data, 
    .scrp_tb = .scrp_table) %>% 
    set_names(names(.scrp_data)) -> res_scrp
  
  attr(res_scrp, "season") <- attr(.scrp_data,"season")
  attr(res_scrp, "week") <- attr(.scrp_data,"week")
  
  return(res_scrp)
}
  
  
  