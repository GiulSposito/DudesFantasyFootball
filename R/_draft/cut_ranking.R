rank_pts <- week_points %>% 
  group_by(teamId) %>% 
  arrange(teamId, week) %>% 
  mutate(cumpts = cumsum(pts)) %>% 
  ungroup() %>% 
  arrange(week, desc(pts)) %>% 
  mutate(rank=rep(1:14,.week)) %>% 
  rename(team=name)

cut_rank <- rank_pts

for(dropWeek in 1:7){

  dropId <- cut_rank %>% 
    filter(
      week==dropWeek,
      filter(pts==min(pts)) 
    ) %>% 
    pull(teamId)
  
  cut_rank <- cut_rank %>% 
    filter( !(teamId==dropId & week>dropWeek) )
  
} 
    
cut_rank <- cut_rank %>% 
  group_by(week) %>% 
  arrange(desc(pts)) %>% 
  mutate(rank=row_number())
  
cut_rank %>% 
  ggplot(aes(x=week, y=reorder(rank,-rank), group=team)) +
  geom_line(aes(color=team), size=2) +
  geom_image(aes(image=imageUrl), size=.04) +
  geom_text(aes(label=pts), size=2.5, color="black", nudge_y = -.40) +
  scale_colour_manual(values = TEAMS_COLOR_SCALE) +
  ylab("rank") +
  theme_void() +
  theme(legend.position = "bottom")


