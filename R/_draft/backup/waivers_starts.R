library(tidyverse)
library(glue)

#.team <- "Limeira Dead Rabbits"
.team <- "Amparo Bikers"
#.team <- "Indaiatuba Riders"
.week <- 10

players <- readRDS(glue("./data/week{.week}_players_projections.rds")) %>% 
  filter(
    fantasy.team %in% c(.team,"*FreeAgent"),
    week==.week
  )
  
# 9:12 %>% 
#   paste0("./data/week",.,"_players_projections.rds") %>% 
#   map_df(readRDS)

# starts
starters <- tibble(
  pos=c("QB","RB","WR","TE","K","DST"),
  qtd=c(1,2,2,1,1,1)
) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      # filter(!id %in% c(13604,8153, 530) ) %>%  # barkley 13604
      filter(position==.x$pos) %>%
      #filter(is.na(injuryStatus)) %>% 
      top_n(.x$qtd, ceiling)
  }, .players=players)

starters <- players %>% 
  # filter(!id %in% c(13604,8153, 530) ) %>%  # barkley 13604
  filter(pos %in% c("WR","RB")) %>% 
  filter(is.na(injuryGameStatus)) %>% 
  anti_join(starters) %>% 
  top_n(1, ceiling) %>% 
  bind_rows(starters,.)

## bench
bench <- tibble(
    pos=c("QB","RB","WR","TE"),
    qtd=c(1,2,2,1)
  ) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      filter(position==.x$pos) %>% 
      top_n(.x$qtd, ceiling)
  }, .players = anti_join(players, starters) )

# releases
releases <- players %>% 
  filter(fantasy.team==.team) %>% 
  anti_join(starters) %>% 
  anti_join(bench)

starters %>% select(first_name, last_name, position, team, fantasy.team)
bench  %>% select(first_name, last_name, position, team, fantasy.team)
releases %>% select(first_name, last_name, position, team, fantasy.team)

starters %>% 
  select(id, first_name, last_name, position, floor, points, ceiling, fantasy.team) %>% 
  inner_join(select(my_player_ids, id, playerId=nfl_id)) %>% 
  inner_join(players_stats, by="playerId") %>% 
  unnest(weekPts) %>% 
  filter(week==7) %>% 
  select(id, first_name, last_name, position.x, floor, points, ceiling, weekPts, fantasy.team)
  