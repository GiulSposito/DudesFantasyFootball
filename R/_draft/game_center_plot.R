team_prob <- sim_summary %>%
  select(teamId, teamName=name, winProb, imageUrl)

team_rem <- sim$teams %>% 
  select(teamId, rosters, season.stats) %>% 
  unnest(c(rosters, season.stats)) %>% 
  filter(rosterSlotId < 20) %>% 
  group_by(teamId, record) %>% 
  summarise(teamRemaining=sum(isEditable), .groups="keep") %>% 
  ungroup() %>% 
  mutate(record = str_remove(record, "-0")) %>% 
  select(teamId, record, teamRemaining) %>% 
  inner_join(team_prob, by="teamId")

game_center <- games_summary %>% 
  select(game.nickname, away.nickname, away.projPts, away.pts, away.teamId, away.name,
         home.pts, home.projPts, home.nickname, home.teamId, home.name) %>% 
  left_join(set_names(team_rem, paste0("home.",names(team_rem))), by = "home.teamId") %>% 
  left_join(set_names(team_rem, paste0("away.",names(team_rem))), by = "away.teamId")

game_center %>% 
  mutate(game.title=paste0("[", away.record,"] ", away.name, " @ ", home.name, " [", home.record, "]")) %>% 
  ggplot(aes(x=0)) +
  # projecao
  geom_col(aes(y=home.projPts), fill="grey", width=0.07) +
  geom_point(aes(y=home.projPts), color="grey", size=5) +
  geom_text(aes(y=home.projPts, label=round(home.projPts, 1)), color="black", vjust=.3, nudge_y = 16, fontface="bold") +
  geom_col(aes(y=-away.projPts), fill="grey", width=0.07) +
  geom_point(aes(y=-away.projPts), color="grey", size=5) +
  geom_text(aes(y=-away.projPts, label=round(away.projPts, 1)), color="black", vjust=.3, nudge_y = -16, fontface="bold") +
  # pontuacao
  geom_col(aes(y=home.pts), fill="red", width=0.07) +
  geom_point(aes(y=home.pts), color="red", size=5) +
  geom_text(aes(y=home.pts, label=round(home.pts, 1)), color="red", vjust=-1, nudge_y = 14, fontface="bold") +
  geom_text(aes(y=home.pts, label=paste0("(",9-home.teamRemaining,"/9)")), color="red", vjust=-1.5, nudge_y = 4, size=3 ) +
  geom_col(aes(y=-away.pts), fill="blue", width=0.07) +
  geom_point(aes(y=-away.pts), color="blue", size=5) +
  geom_text(aes(y=-away.pts, label=round(away.pts, 1)), color="blue", vjust=-1, nudge_y = -14, fontface="bold") +
  geom_text(aes(y=-away.pts, label=paste0("(",9-away.teamRemaining,"/9)")), color="blue", vjust=-1.5, nudge_y = -4, size=3 ) +
  # decorators
  #geom_text(aes(y=0, label=game.title), vjust=-3, fontface="bold") +
  xlim(-.2,.2) +
  # facet
  facet_wrap(~game.title, ncol=1) + 
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_text(face="bold", margin = margin(5,1,5,1,"pt"), size=12)
  )



