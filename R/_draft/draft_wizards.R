library(tidyverse)
library(glue)

matchups <- c(1,8) %>% 
  paste0("./data/week",.,"_matchups_json.rds") %>% 
  map(readRDS)

team.players <- matchups %>% 
  map(function(matchup){
    matchup %>% 
      map_df(function(mtch){
        cleanTeam <- . %>%
          select(team.id=id, team.name=name, players) %>% 
          mutate(players = map(players, ~select(.x, id, name, position, rosterSlot)))
        
        resp <- bind_rows(
          cleanTeam(mtch$leagues$matchup$awayTeam),
          cleanTeam(mtch$leagues$matchup$homeTeam)
        ) %>% 
          as_tibble() %>% 
          set_names(c("team.id","team.name", paste0("week",mtch$leagues$matchup$week,".players")))
      })
  })

common.rosters <- team.players[[1]] %>% 
  inner_join(team.players[[2]], by = c("team.id", "team.name")) %>% 
  mutate( 
    common.players = map2(week1.players, week8.players, function(w1,w2){
      w1 %>% inner_join(select(w2,id), by="id")}),
    released.players = map2(week1.players, common.players, function(w1,wc){
      w1 %>% anti_join(select(wc,id), by="id")}),
  )

player.points <- readRDS("./data/2018/players_points.rds") %>%
  filter(week <= 8) %>% 
  group_by(src_id) %>% 
  summarise(points=sum(points, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(id=as.character(src_id))

players <- common.rosters %>% 
  mutate( 
    common.players = map(
      common.players,
      function(.rst,.pts) {inner_join(.rst, .pts, by="id")},
      .pts = player.points
    ),
    released.players = map(
      released.players,
      function(.rst,.pts) {inner_join(.rst, .pts, by="id")},
      .pts = player.points
    ))

players.report <- players %>% 
  mutate(
    original.number = map_int(common.players, nrow),
    released.number = map_int(released.players, nrow)
  ) %>% 
  mutate(
    original.points = map_dbl(common.players, ~sum(.x$points)),
    released.points = map_dbl(released.players, ~sum(.x$points))
  )

players.report %>% 
  select(-contains("players")) %>% 
  arrange(desc(original.points))

players.report %>% 
  mutate( top.original.player = map(common.players, filter, points==max(points)),
          top.released.player = map(released.players, filter, points==max(points))) %>% 
  unnest( top.original.player, top.released.player, .sep = ".") %>% 
  select( team.id, team.name, contains("top"), -contains("id")) %>%
  View()
  


