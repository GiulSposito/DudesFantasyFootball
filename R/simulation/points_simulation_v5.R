library(tidyverse)

simulateGames <- function(.week, .season, .ptsproj, .matchup_games, .teams_rosters, .players_stats, .players_id) {
  
  # .week     <- week
  # .season   <- season
  # .ptsproj  <- ptsproj
  # .matchups <- matchups_games
  # .teams    <- teams_rosters
  # .plstats  <- players_stats
  # .plids    <- player_ids
  
  # so colunas de ids dos jogos
  mtch <- .matchup_games %>% 
    select(matchupId, week, awayTeam.teamId, homeTeam.teamId) %>% 
    filter(week==.week)
  
  # unest o team roster
  tms <- .teams_rosters %>% 
    select(teamId, teamName=name, rosters) %>% 
    unnest(rosters) %>% 
    filter(week==.week)
  
  # filtra estatisticas dos  jogadores
  # stats <- .players_stats %>% 
  #   select(playerId, name, position, byeWeek, injuryGameStatus, weekPts) %>% 
  #   unnest(weekPts) %>% 
  #   filter(week==.week) 
  
  # estrutura de projecao dos jogadores
  projs <- .ptsproj %>% 
    inner_join(select(.players_id, id, playerId=nfl_id), by="id") %>% 
    filter(week==.week, season==.season) %>% 
    select(id, playerId, pts.proj) %>% 
    group_by(id, playerId) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(pts.proj = map(data, ~.x$pts.proj )) %>% 
    select(-data)
  
  # tamanho da simulacao
  SIMULATION_SIZE = 1000
  
  # simulação dos jogadores
  players_sim <- tms %>% 
    inner_join(projs, by="playerId") %>% 
    mutate( simulation = map(pts.proj, sample, size=SIMULATION_SIZE, replace=T))
  
  # pega os titulares e soma para obter a pontuacao dos jogos
  teams_sim <- players_sim %>% 
    # inner_join(stats, by="playerId") %>% 
    filter( rosterSlotId != 20 ) %>%  # rosterId==20 indica banco
    select(teamId, playerId, simulation) %>% 
    group_by(teamId) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(simulation = map(data, function(.sim){
      # cada jogador tem um array de pontos projetados 
      # extrai como uma lista (cada jogador uma linha)
      # junta como uma matriz
      # soma as colunas (cada coluna é uma simulação)
      .sim %>% 
        pull(simulation) %>% 
        do.call(rbind, .) %>% 
        colSums() %>%
        return()    
    })) %>% 
    select(-data)
  
  # pivot para longo para fazer o bind da simulacao mais facil
  matchup_sim <- mtch %>% 
    pivot_longer(c(-matchupId, -week), names_to="name", values_to="teamId") %>% 
    mutate(name=str_remove(name, "\\.teamId")) %>% 
    inner_join(teams_sim, by="teamId") %>% 
    # uma vez feito o join, pivot para wide de volta
    pivot_wider(id_cols = c(matchupId, week), names_from="name", values_from=c(teamId, simulation), names_sep=".") %>% 
    # prefico identificar a entidade
    set_names(c("matchupId", "week", "awayTeam.teamId", "homeTeam.teamId", "awayTeam.simulation", "homeTeam.simulation")) %>% 
    # cada time tem um array de pontos da simulacao dos titulares
    mutate( 
      # verifica pontuacao do home é maior que o visitante para saber se o home ganhou
      homeTeam.win = map2(awayTeam.simulation, homeTeam.simulation, function(.sat, .sht) .sht>.sat), 
      # se home ganhou visitante perdeu
      awayTeam.win = map(homeTeam.win, function(.wht) !.wht ), 
      # diferenca entre pontos (na direcao do home)
      homeTeam.ptsdiff = map2(awayTeam.simulation, homeTeam.simulation, function(.sat, .sht) .sht-.sat),
      # probabilidade de ganhar (media das vitorias)
      homeTeam.winProb = map_dbl(homeTeam.win, mean), 
      awayTeam.winProb = map_dbl(awayTeam.win, mean),
      # pontos projetado (mediana da simulacao)
      homeTeam.totalPts = map_dbl(homeTeam.simulation, median),
      awayTeam.totalPts = map_dbl(awayTeam.simulation, median)
    )
  
  # retorna a estrutura de calculo toda, mais facil para montar o report
  list(
    week     = .week,
    season   = .season,
    ptsproj  = .ptsproj,
    matchups = .matchup_games,
    teams    = .teams_rosters,
    players_stats  = .players_stats,
    players_id     = .players_id,
    players_sim    = players_sim,
    teams_sim      = teams_sim,
    matchup_sim    = matchup_sim
  ) %>% 
    return()
}




