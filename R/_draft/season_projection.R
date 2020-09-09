projs <- proj_df %>% 
  add_player_info() %>% 
  mutate(id=as.integer(id))

drafted_season_proj <- teams_rosters %>% 
  select(teamId, name, rosters) %>% 
  unnest(rosters) %>% 
  inner_join(select(player_ids, id, nfl_id), by=c("playerId"="nfl_id")) %>% 
  inner_join(projs, by = "id") %>% 
  filter(avg_type=="weighted") %>% 
  select(teamId, name, id, first_name, last_name, position, avg_type, floor, points, ceiling) %>% 
  group_by(teamId, name) %>% 
  summarise(
    floor = sum(floor, na.rm = T),
    points = sum(points, na.rm = T),
    ceiling = sum(ceiling, na.rm = T)
  ) %>% 
  arrange( desc(points) ) 

saveRDS(drafted_season_proj, "./data/drafted_season_projection.rds")
