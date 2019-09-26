library(tidyverse)
library(glue)

.team <- "Bikers"
.week <- 4

players <- readRDS(glue("./data/week{.week}_players_projections.rds")) %>% 
  filter(
    fantasy.team %in% c("*FreeAgent",.team),
    !(team %in% c("FA", "FA*","NYJ","SFO"))
  ) 

# starts
starters <- tibble(
  pos=c("QB","RB","WR","TE","K","DEF"),
  qtd=c(1,2,2,1,1,1)
) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      # filter(!id %in% c(13604,8153, 530) ) %>%  # barkley 13604
      filter(position==.x$pos) %>% 
      top_n(.x$qtd, ceiling)
  }, .players=players)

starters <- players %>% 
  # filter(!id %in% c(13604,8153, 530) ) %>%  # barkley 13604
  filter(pos %in% c("WR","RB")) %>% 
  anti_join(starters) %>% 
  top_n(1, ceiling) %>% 
  bind_rows(starters,.)

## bench
bench <- tibble(
    pos=c("QB","RB","WR","TE"),
    qtd=c(1,2,2,1)
  ) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, floor){
    .players %>% 
      filter(position==.x$pos) %>% 
      top_n(.x$qtd, ceiling)
  }, .players = anti_join(players, starters) )

# releases
releases <- players %>% 
  filter(fantasy.team==.team) %>% 
  anti_join(starters) %>% 
  anti_join(bench)

starters
bench 
releases

