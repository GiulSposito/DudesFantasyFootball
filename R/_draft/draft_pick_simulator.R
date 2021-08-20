players <- season_proj %>% 
  mutate(playerId = as.integer(id)) %>% 
  relocate(playerId, everything()) %>% 
  filter(avg_type=="average")

# 10 times + 1 QB + 2 WR + 2 RB + 1 TE + 1 K + 1 DST + 1 W/R
# 10 * (1+2+2+1+1+1)

rosters <- tibble(
  teamId = 1:10,
  slots = list(
    tibble(
        slotId   = 1:8,
        position = c("QB","WR","WR","RB","RB","TE","K","DST"),
        playerId = as.integer(NA)
      )
    )
  ) %>% unnest(slots)

# snake 3RR
picks <- tibble(
  pick = 1:80,
  teamId = c(1:10,10:1,10:1,rep(c(1:10,10:1),2), 1:10),
  playerId = as.integer(NA)
)


# snake 
picks <- tibble(
  pick = 1:80,
  teamId = rep(c(1:10,10:1),4),
  playerId = as.integer(NA)
)


# para todo o pick
for(p in 1:80){

  # time da vez
  pteam <- picks[p,]$teamId
  
  # slots disponiveis no time da vez
  avail_slots <- rosters %>% 
    filter(teamId==pteam, is.na(playerId)) %>% 
    select(-playerId)
  
  # jogadores disponiveis
  avail_players <- players %>% 
    anti_join(rosters, by="playerId") %>% 
    arrange(desc(points))
    
  # melhor pick
  best_pick <- avail_players %>% 
    inner_join(avail_slots, by="position") %>% 
    arrange(desc(points_vor)) %>% 
    head(1) %>%
    select(teamId, slotId, pickPos=position, pickId = playerId)
    
  # preenche o slot com o id do melhor pick
  rosters <- rosters %>% 
    left_join(best_pick, by = c("teamId", "slotId")) %>% 
    mutate(playerId=if_else(!is.na(playerId),playerId,pickId)) %>% 
    select(-pickPos, -pickId)
  
  # preenche a escolha do pick
  picks[p,]$playerId <- best_pick$pickId[1]
    
}

rosters %>% 
  inner_join(select(picks, playerId, pick), by="playerId") %>% 
  inner_join(select(players, playerId, first_name, last_name, team, floor,points,ceiling), by="playerId") %>% 
  group_by(teamId) %>% 
  summarise(
    floor=sum(floor),
    points=sum(points),
    ceiling=sum(ceiling)
  ) %>% 
  ggplot(aes(x=as.factor(teamId),y=points)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=floor, ymax=ceiling)) +
  ylim(0,NA) +
  theme_minimal()
