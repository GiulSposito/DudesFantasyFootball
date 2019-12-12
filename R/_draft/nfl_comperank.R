# install.packages("comperank")
library(comperes)
library(tidyverse)
library(comperank)

gm_res <- readRDS("./data/post_matchups_results.rds")

getTeam <- function(.team, .prefix){
  .team %>% 
    select(id, name, pts) %>% 
    mutate(id = as.integer(id), pts=as.numeric(pts)) %>% 
    set_names(paste0(.prefix,".",names(.)))
}

games <- gm_res %>% 
  map_df(
    function(.weekgames){
      map_df(.weekgames,
             function(.game)
               bind_cols(
                 getTeam(.game$leagues$matchup$awayTeam, "away"),
                 getTeam(.game$leagues$matchup$homeTeam, "home")
               ),
             .id="week.game.id"
      )
    },
    .id="week"
  ) %>% 
  mutate(game.id = as.integer(paste0(week, week.game.id))) %>% 
  select(-week, -week.game.id) %>% 
  as_tibble()

games.long <- bind_rows(
  select(games, game=game.id, player=home.name, score=home.pts),
  select(games, game=game.id, player=away.name, score=away.pts)
) 

games.long %>% 
  h2h_long()

games.long %>% 
  rank_colley(keep_rating = T) %>% 
  arrange(ranking_colley)
