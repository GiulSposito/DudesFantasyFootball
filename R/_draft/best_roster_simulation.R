library(tidyverse)
library(glue)

simBestTeam <- function(WEEK){

  sim <- readRDS(glue("./data/simulation_v5_week{WEEK}_final.rds"))
  
  plyr  <- sim$players_stats
  
  mtchs <- sim$matchups  %>% 
    select(matchupId, awayTeam.teamId, awayTeam.outcome, homeTeam.teamId, homeTeam.outcome) %>% 
    filter(awayTeam.teamId==4 | homeTeam.teamId==4)
  
  OPPID <- ifelse( mtchs$awayTeam.teamId==4, mtchs$homeTeam.teamId,
                   mtchs$awayTeam.teamId )
  
  psim <- sim$players_sim
  tsim <- sim$teams_sim
  
  roster <- psim %>%
    filter(teamId==4, rosterSlotId<=20, is.na(injuryGameStatus)) %>% 
    select(teamId, rosterSlotId, playerId, byeWeek, pts.proj, simulation.org, weekPts) %>% 
    inner_join(select(plyr, playerId, name, position), by="playerId")
  
  avRoster <- roster %>% 
    select(position, playerId) %>% 
    split(as.factor(.$position)) %>% 
    map(~pull(.x, playerId))
  
  rostersComb <-  crossing(
    QB = avRoster[["QB"]],
    WR1 = avRoster[["WR"]],
    WR2 = avRoster[["WR"]],
    RB1 = avRoster[["RB"]],
    RB2 = avRoster[["RB"]],
    TE  = avRoster[["TE"]],
    K   = avRoster[["K"]],
    DEF = avRoster[["DEF"]],
    WR  = c(avRoster[["WR"]], avRoster[["RB"]])
  ) %>% 
    filter( WR1!=WR2, RB1!=RB2 ) %>% 
    filter( WR!=WR1, WR!=WR2, WR!=RB1, WR!=RB2 ) %>% 
    distinct() %>% 
    mutate(combId= row_number()) %>% 
    pivot_longer(-combId, names_to="position", values_to="playerId")
  
  combSim <- rostersComb %>% 
    inner_join(select(roster,playerId, weekPts,simulation.org), by="playerId") %>% 
    group_by(combId) %>% 
    nest() %>% 
    mutate(teamSim = map(data, function(tRoster){
      tRoster %>% 
        pull(simulation.org) %>%
        unlist() %>% 
        matrix(nrow = nrow(tRoster)) %>% 
        colSums() %>% 
        return()
    })) 
  
  
  opp <- tsim %>% 
    filter(teamId==OPPID) %>% 
    pull(simulation.org) %>% 
    .[[1]]
  
  combWinner <- combSim %>% 
    mutate( oppSim = list(opp) ) %>%
    mutate( teamPts = map_dbl(teamSim, mean, na.rm=T)) %>% 
    mutate( mtchSim = map2_dbl(teamSim, oppSim, function(tSim,oSim){
      mean(1*(tSim>oSim))
    })) %>% 
    arrange(desc(teamPts),desc(mtchSim))
  
  newPts <- combWinner[1,] %>% 
    select(-teamSim, -oppSim) %>%  
    #filter(combId==4111) %>% 
    unnest(data) %>% 
    pull(weekPts) %>% 
    sum(na.rm=T)
  
  sim$teams %>% 
    unnest(week.stats) %>% 
    select(teamId, name, week, pts) %>% 
    filter(week==WEEK, teamId %in% c(4,OPPID) ) %>% 
    mutate( newPts = newPts ) %>% 
    return()
}

rostersSims <- 1:13 %>% 
  map_df(simBestTeam)

rostersSims %>% 
  mutate(worst = newPts == pts) %>% 
  filter(teamId==4)
 