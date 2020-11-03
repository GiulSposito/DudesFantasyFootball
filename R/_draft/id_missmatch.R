sim$ptsproj %>% 
  filter(week==7) %>% 
  filter(!str_detect(data_src, "ERROR")) %>% 
  filter(!data_src %in% c("floor","ceiling")) %>% 
  add_count(id) %>% 
  filter( pts.proj > 1 ) %>% 
  arrange(id, n)
  

sim$ptsproj %>% 
  select(data_src) %>% 
  distinct() %>% 
  filter(!str_detect(data_src, "ERROR")) %>% 
  filter(!data_src %in% c("floor","ceiling"))
  
  
sim$ptsproj %>% 
  filter(week==7) %>% 
  filter(!str_detect(data_src, "ERROR")) %>% 
  filter(!data_src %in% c("floor","ceiling")) %>% 
  filter(pts.proj > 5) %>% 
  select(id, pos, data_src, pts.proj) %>% 
  distinct() %>% 
  pivot_wider(id_cols=c(id,pos), names_from=data_src, values_from=pts.proj)

pIds <- sim$players_stats %>%
  unnest(weekPts) %>% 
  filter(week==7) %>% 
  select(playerId, name, position, points = weekSeasonPts) %>% 
  left_join(rename(my_player_ids, playerId=nfl_id), by="playerId")

# jogadores sem nenhuma importacao
pIds %>% 
  filter(is.na(id)) %>% 
  # filter(position=="WR") %>% 
  select(playerId, name, position, points) %>% 
  arrange(desc(points))   

# Tua?
pIds %>% 
  filter(str_detect(name, "Tua"))

# defesas?
sim$ptsproj %>% 
  filter(week==7) %>% 
  filter(!str_detect(data_src, "ERROR")) %>% 
  filter(!data_src %in% c("floor","ceiling")) %>% 
  filter(pts.proj > 5) %>% 
  select(id, pos, data_src, pts.proj) %>% 
  distinct() %>% 
  pivot_wider(id_cols=c(id,pos), names_from=data_src, values_from=pts.proj) %>% 
  filter(pos=="DST") %>% 
  left_join(pIds, by="id") %>% 
  filter(is.na(NFL)) %>% 
  View

nflScrap %>% 
  filter(position=="DST") %>% 
  glimpse()

scraps["DST"] %>% 
  `attr<-`("season", 2020) %>% 
  `attr<-`("week", 7) %>% 
  #filter(data_src=="NFL") %>% 
  # calcPlayersProjections(read_yaml("./config/score_settings.yml"))
  calcPointsProjection(2020, read_yaml("./config/score_settings.yml"))


