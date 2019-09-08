# SCRAP
scraps[["QB"]] %>% 
  filter(team=="ARI", data_src=="NumberFire") %>%
  mutate(id=as.integer(id)) %>%
  glimpse()

# PLAYER ID 
player_ids %>%
  filter(numfire_id=="kyler-murray") %>% 
  glimpse()
  
# FANTASY
x <- matchups %>% 
  map(~as_tibble(.x[[1]]))
x[[2]]$matchup$awayTeam$players[[1]] %>% 
  filter(pos)

