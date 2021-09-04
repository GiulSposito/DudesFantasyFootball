library(tidyverse)

season_proj <- readRDS("./data/season_projtable.rds")

players <- season_proj %>% 
  mutate(playerId = as.integer(id)) %>% 
  relocate(playerId, everything()) %>% 
  filter(avg_type=="average")


season_proj %>% 
  filter(avg_type=="average", position=="TE") %>% 
  arrange(desc(ceiling)) %>% 
  View("bench")

# 10 times + 1 QB + 2 WR + 2 RB + 1 TE + 1 K + 1 DST + 1 W/R
# 10 * (1+2+2+1+1+1)

rosters <- tibble(
  teamId = 1:14,
  slots = list(
    tibble(
        slotId   = 1:9,
        position = c("QB","WR","WR","RB","RB","TE","K","DST","W/R"),
        playerId = as.integer(NA)
      )
    )
  ) %>% unnest(slots)

# snake 3RR
# picks <- tibble(
#   pick = 1:90,
#   teamId = c(1:10,10:1,10:1,rep(c(1:10,10:1),3)),
#   playerId = as.integer(NA)
# )

# snake 
picks <- tibble(
  pick = 1:126,
  teamId = c(rep(c(1:14,14:1),4),1:14),
  playerId = as.integer(NA)
)


# para todo o pick
for(p in 1:126){

  # time da vez
  pteam <- picks[p,]$teamId
  
  # slots disponiveis no time da vez
  avail_slots <- rosters %>% 
    filter(teamId==pteam, is.na(playerId)) %>% 
    select(-playerId)
  
  # trata a posicao coringa
  if (9 %in% avail_slots$slotId) {
    
    avail_slots <- tibble(
      teamId=c(pteam,pteam),
      slotId=c(9,9),
      position=c("WR","RB")
    ) %>% 
      bind_rows(avail_slots) %>% 
      filter(position!="W/R") %>% 
      arrange(slotId)
    
  }
  
  # jogadores disponiveis
  avail_players <- players %>% 
    anti_join(rosters, by="playerId") %>% 
    arrange(desc(points))
    
  # melhor pick
  best_pick <- avail_players %>% 
    inner_join(avail_slots, by="position") %>% 
    arrange(desc(floor_vor), slotId) %>% 
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
  inner_join(select(players, playerId, first_name, last_name, pos=position, team, floor,points,ceiling), by="playerId") %>% 
  group_by(teamId) %>% 
  summarise(
    floor=sum(floor),
    points=sum(points),
    ceiling=sum(ceiling)
  ) %>% 
  ungroup() %>% 
  arrange(desc(points))
  ggplot(aes(x=as.factor(teamId),y=points)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=floor, ymax=ceiling), width=.3) +
  ylim(0,NA) +
  labs( title="Projeção de Pontos Temporada", 
        subtitle="Full PPR | Snake Draft | VOR",
        x="Team Draft Position",
        y="Fantasy Points") +
  theme_minimal()


rosters %>% 
  inner_join(select(picks, playerId, pick), by="playerId") %>% 
  inner_join(select(players, playerId, first_name, last_name, pos=position, team, floor,points,ceiling), by="playerId") %>% 
  mutate(teamId = as.factor(teamId)) %>% 
  select(pick, teamId, pos, points) %>% 
  ggplot(aes(x=pick, y=points, color=teamId, shape=pos)) +
  geom_point() +
  labs( title="Projeção de Pontos Temporada", 
        subtitle="Full PPR | Snake Draft | VOR",
        x="Draft Pick",
        y="Fantasy Points") +
  theme_minimal()
