library(tidyverse)

players <- readRDS("./data/week4_players_projections.rds") %>% 
  filter(
    fantasy.team %in% c("*FreeAgent","Bikers"),
    !(team %in% c("FA", "FA*"))
  ) 

# barkley 13604
players %>% 
  filter(team=="NYG", position=="RB")

# barkley 13604
# Gould 8153
# SF Def 530
players %>% 
  filter(team=="SFO", position=="DST")



# starts
starters <- tibble(
  pos=c("QB","WR","RB","TE","K","DST"),
  qtd=c(1,2,2,1,1,1)
) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      # filter(!id %in% c(13604,8153, 530) ) %>%  # barkley 13604
      filter(position==.x$pos) %>% 
      top_n(.x$qtd, floor)
  }, .players=players)

starters <- players %>% 
  # filter(!id %in% c(13604,8153, 530) ) %>%  # barkley 13604
  filter(pos %in% c("WR","RB")) %>% 
  anti_join(starters) %>% 
  top_n(1, floor) %>% 
  bind_rows(starters,.)

starters 

## bench
tibble(
    pos=c("K","QB","WR","RB","TE"),
    qtd=c(1,1,2,2,1)
  ) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      # filter(position==.x$pos, team !="MIA") %>% 
      top_n(.x$qtd, ceiling)
  }, .players = anti_join(players, starters) )

