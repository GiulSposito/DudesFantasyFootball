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



ptsproj %>% 
  bind_rows(projectErrorPoints(players_stats, ptsproj, my_player_ids, 2)) %>% 
  filter(week==2, id==505)

players_stats %>% 
  filter(lastName=="Adam")
