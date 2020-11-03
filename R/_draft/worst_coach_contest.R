library(tidyverse)
library(glue)

resp <- 1:7 %>% 
  map_df(function(.week){
    sim <- readRDS(glue("./data/simulation_v5_week{.week}_final.rds"))
    
    roster <- sim$teams %>% 
      unnest(rosters) %>% 
      select(teamId, teamName=name, imageUrl, rosterSlotId, playerId) %>% 
      mutate(week=.week)
    
    players <- sim$players_stats %>% 
      select(playerId, playerName=name, position, weekPts) %>% 
      unnest(weekPts) %>% 
      filter(week==.week)
    
    teamRoster <- inner_join(roster, players, by=c("playerId","week"))
    
    teamPoints <- teamRoster %>% 
      filter(rosterSlotId<20) %>% 
      group_by(teamId, teamName, imageUrl) %>% 
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


TEAMS_COLOR_SCALE <- c("#B3995D","black","#03202F","#0B2265","#fb9a99","#A5ACAF","#0085CA",
                       "#ff7f00","#203731","#6a3d9a","#A71930","#A71930","#FFB612","#004953")


resp %>%
  group_by(week) %>% 
  arrange(week, gain, pointsMade) %>% 
  mutate(wrank=1:14) %>% 
  ungroup() %>%
  group_by(teamId) %>% 
  arrange(week) %>% 
  mutate(
    cum_gain=round(cumsum(gain),1),
    cum_pointsMade=cumsum(pointsMade)
  ) %>% 
  ungroup() %>% 
  group_by(week) %>% 
  arrange(week, cum_gain, cum_pointsMade) %>% 
  mutate(rank=1:14) %>% 
  ungroup() %>% 
  ggplot(aes(x=week, y=reorder(rank,-rank), group=teamName)) +
  geom_line(aes(color=teamName), size=2) +
  geom_image(aes(image=imageUrl), size=.04) +
  geom_text(aes(label=cum_gain), size=2.5, color="black", nudge_y = -.33) +
  scale_colour_manual(values = TEAMS_COLOR_SCALE) +
  ylab("rank") +
  theme_void()
