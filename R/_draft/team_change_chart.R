library(tidyverse)
library(glue)
library(ggimage)


teams_changes_balance <- 2:7 %>% 
  map_df(function(week){
    w1 = week-1
    w2 = week
    
    sim_w1 <- readRDS(glue("./data/simulation_v5_week{w1}_final.rds"))
    sim_w2 <- readRDS(glue("./data/simulation_v5_week{w2}_final.rds"))
    
    team_w1 <- sim_w1$teams %>% 
      unnest(rosters) %>% 
      select(teamId, playerId)
    
    team_w2 <- sim_w2$teams %>% 
      unnest(rosters) %>% 
      select(teamId, playerId)
    
    mantidos <- team_w1 %>% 
      inner_join(team_w2, by = c("teamId", "playerId"))
    
    liberados <- team_w1 %>% 
      anti_join(mantidos, by = c("teamId", "playerId")) %>% 
      mutate( status = "liberado" )
    
    contratados <- team_w2 %>% 
      anti_join(mantidos, by = c("teamId", "playerId")) %>% 
      mutate( status = "contratado" )
    
    changes <- bind_rows(liberados, contratados)
    
    # saldo
    changed_teams <- sim_w2$players_stats %>% 
      unnest(weekPts) %>% 
      filter(week==w2) %>% 
      select(playerId, name, weekPts) %>% 
      inner_join(changes, by = "playerId") %>% 
      mutate( weekPts = if_else(status=="contratado", weekPts, -weekPts)) %>% 
      group_by(teamId) %>% 
      summarise(
        balance = sum(weekPts, na.rm = T),
        changes=n(),
        .groups="drop"
      ) %>% 
      inner_join(select(sim_w1$teams, teamId, name, imageUrl), by = "teamId") %>% 
      select(teamId, name, imageUrl, balance, changes)
    
    sim_w2$teams %>% 
      select(teamId, name, imageUrl) %>% 
      anti_join(changed_teams, by="teamId") %>% 
      mutate(balance=0, changes=0) %>% 
      bind_rows(changed_teams) %>% 
      arrange(desc(balance)) %>% 
      mutate(
        wrank=1:nrow(.),
        week=w2
      )
  })

rank_history <- teams_changes_balance %>% 
  group_by(teamId) %>% 
  arrange(week) %>% 
  mutate( cum_balance = cumsum(balance) ) %>% 
  ungroup() %>% 
  group_by(week) %>% 
  arrange(desc(cum_balance)) %>% 
  mutate(rank = 1:14) %>% 
  ungroup() %>% 
  arrange(week, rank) %>% 
  rename(team=name)


TEAMS_COLOR_SCALE <- c("#B3995D","black","#03202F","#0B2265","#fb9a99","#A5ACAF","#0085CA",
                       "#ff7f00","#203731","#6a3d9a","#A71930","#A71930","#FFB612","#004953")


rank_history %>% 
  ggplot(aes(x=week, y=reorder(rank,-rank), group=team)) +
  geom_line(aes(color=team), size=2) +
  geom_image(aes(image=imageUrl), size=.04) +
  geom_text(aes(label=cum_balance), size=2.5, color="black", nudge_y = -.33) +
  scale_colour_manual(values = TEAMS_COLOR_SCALE) +
  ylab("rank") +
  theme_void()



# changes  
# sim_w2$players_stats %>% 
#   unnest(weekPts) %>% 
#   filter(week==w2) %>% 
#   select(playerId, name, weekPts) %>% 
#   inner_join(changes) %>% 
#   mutate( weekPts = if_else(status=="contratado", weekPts, -weekPts)) %>% 
#   filter(teamId==7) %>% 
#   arrange(weekPts)
