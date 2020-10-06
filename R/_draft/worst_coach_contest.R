library(tidyverse)
library(glue)

resp <- 1:4 %>% 
  map_df(function(.week){
    sim <- readRDS(glue("./data/simulation_v5_week{.week}_posMNF.rds"))
    
    roster <- sim$teams %>% 
      unnest(rosters) %>% 
      select(teamId, teamName=name, rosterSlotId, playerId) %>% 
      mutate(week=.week)
    
    players <- sim$players_stats %>% 
      select(playerId, playerName=name, position, weekPts) %>% 
      unnest(weekPts) %>% 
      filter(week==.week)
    
    teamRoster <- inner_join(roster, players, by=c("playerId","week"))
    
    teamPoints <- teamRoster %>% 
      filter(rosterSlotId<20) %>% 
      group_by(teamId, teamName) %>% 
      summarise( pointsMade = sum(weekPts, na.rm = T))
    
    
    bestTeam <- teamRoster$teamId %>% 
      unique() %>% 
      map_df(function(.id, .tm){
        .tr <- filter(.tm, teamId==.id)
        
        st1 <- tibble(
          pos=c("QB","RB","WR","TE","K","DEF"),
          qtd=c(1,2,2,1,1,1)
        ) %>% 
          split(1:nrow(.)) %>% 
          map_df(function(.x,.tr){
            .tr %>%
              filter(position==.x$pos) %>%
              top_n(.x$qtd, weekPts) %>%
              return()
          },.tr=.tr)
        
        .tr %>% 
          anti_join(st1) %>% 
          filter(position %in% c("RB","WR")) %>% 
          top_n(1, weekPts) %>% 
          bind_rows(st1) %>% 
          return()
        
      }, .tm=teamRoster)
    
    bestTeamPoints <- bestTeam %>% 
      group_by(teamId, teamName) %>% 
      summarise(bestPoints=sum(weekPts, na.rm = T))
    
    teamPoints %>% 
      inner_join(bestTeamPoints) %>% 
      mutate(gain=bestPoints-pointsMade) %>% 
      mutate(week=.week) %>% 
      return()
  
        
  })


resp %>% 
  group_by(teamId, teamName) %>% 
  summarise(pointsNotGain=sum(gain)) %>% 
  arrange(desc(pointsNotGain))

resp %>% 
  pivot_wider(id_cols = c(teamId, teamName), names_from=week, values_from=gain)
