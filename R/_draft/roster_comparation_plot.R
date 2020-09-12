.week <- 1
# fixed positions
slotLevels <- gl(8,1,labels=c("QB","RB","WR","TE","W/R","K","DEF","BN"))
posLevels <- gl(6,1,labels=c("QB","RB","WR","TE","K","DEF"))

rosterSlots <- tibble(
  rosterSlotId = c(1:5,7,8,20,21, NA),
  rosterSlot   = c("QB","RB","WR","TE","W/R","K","DEF","BN","BN","BN")
)

# seleciona algumas colunas ta tabela de projecao
projections <- sim$proj_table %>% 
  filter(position==pos) %>% 
  select(id, projPts = points, pos_rank, drop_off, sd_pts, floor, ceiling, tier) 

# junta os rosters dos times, estatíticas dos jogadores, a tabela de projecao e
# uma de-para de apreviacao do Injury Status
rosters <- sim$teams %>% 
  # unnest do roster por time
  select(teamId, teamName=name, rosters) %>% 
  unnest(rosters) %>%                                                         # 210 players
  # mapeamento ffa<->nfl
  inner_join(select(sim$players_id, id, playerId=nfl_id), by="playerId") %>%  # 210 players
  # statisticas dos jogadores
  inner_join(sim$players_stats, by="playerId") %>%                            # 210 players]
  # estatisticas podem ter mais de uma semana
  unnest(weekPts) %>% 
  filter(week == .week) %>% 
  # mapeamento da posicao do slot no roster
  inner_join(rosterSlots, by="rosterSlotId") %>% 
  # campos de interesse
  select(id, playerId, teamId, teamName, name, position, nflTeamAbbr, rosterSlot, 
         injuryGameStatus, isEditable, isReserveStatus=isReserveStatus.x, isUndroppable, 
         byeWeek, weekPts, seasonPts) %>% 
  # adiciona as projecoes
  left_join(projections, by="id") %>% 
  # adiciona abreviatura do injury game status
  left_join(readRDS("./data/injuryGameStatusAbbr.rds"), by = "injuryGameStatus")
  # %>% replace_na(list(points=0, floor=0, ceiling=0))

# junta roster, statisticas e projecao
rosters %>% 
  filter(teamId==1) %>% 
  mutate(
    # fixa posicao e slot como fatores fixo para melhorar o gráfico
    position = factor(position, levels=posLevels),
    rosterSlot = factor(rosterSlot, levels=slotLevels)
  ) %>% 
  # decide se em pontos deixa vazio (ainda nao jogou) ou coloca week points (já jogou)
  mutate(
    points = case_when(
      isEditable ~ as.numeric(NA),
      !isEditable ~ weekPts 
    )
  ) %>% 
  # modifica o label do jogador conforme for o status de lesao
  mutate(
    playerLabel = case_when(
      !is.na(injuryGameStatus) ~ paste0(name, " (",injuryAbbr, ")" ," [", rosterSlot,"]"),
      T ~ paste0(name," [", rosterSlot,"]")
    )
  ) %>% 
  arrange(rosterSlot, position) %>% 
  mutate(display.order=1:nrow(.)) %>% 
  ggplot(aes(x=reorder(playerLabel, -display.order), color=position)) +
  geom_pointrange(aes(y=projPts, ymin=floor, ymax=ceiling)) +
  geom_label(aes(y=projPts, label=round(projPts,1)), size = 3,
             label.padding = unit(0.15, "lines"), show.legend = F) +
  geom_point(aes(y=points), size=2, color="black") +
  xlab("") +
  ylab("Fantasy Points") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")


rosters %>% 
  select(injuryGameStatus) %>% 
  distinct() %>% 
  arrange(injuryGameStatus) %>% 
  mutate( injuryAbbr = c("D","IR","Out","Q","CVD", NA)) %>% 
  saveRDS("./data/injuryGameStatusAbbr.rds")
  

