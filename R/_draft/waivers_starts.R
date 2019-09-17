library(tidyverse)

players <- readRDS("./data/week3_players_projections.rds") %>% 
  filter(
    fantasy.team %in% c("*FreeAgent","Bikers"),
    !(team %in% c("FA", "FA*"))
  )

# starts
starters <- tibble(
  pos=c("QB","WR","RB","TE","K","DST"),
  qtd=c(1,2,2,1,1,1)
) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      filter(position==.x$pos) %>% 
      top_n(.x$qtd, floor)
  }, .players=players)

starters <- players %>% 
  filter(pos %in% c("WR","RB")) %>% 
  anti_join(starters) %>% 
  top_n(1, floor) %>% 
  bind_rows(starters,.)

starters %>% 
  filter(position!="DST")

## bench
tibble(
    pos=c("QB","WR","RB","TE"),
    qtd=c(1,2,2,1)
  ) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      filter(position==.x$pos) %>% 
      top_n(.x$qtd, floor)
  }, .players = anti_join(players, starters) )

