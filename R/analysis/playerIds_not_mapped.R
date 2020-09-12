# Script que tenta descobrir os IDs faltantes na tabela players_ids do ffanalytics

# "Steven Sims"
# teamId = 7
# playerId=2563033
# 
# "Slye"
# 2563132

# Slye está na projecao? => SIM
proj_table %>% 
  filter(str_starts(last_name, "Slye") | str_starts(last_name, "Sims"))

# Slye está nas estatíticas? => SIM
players_stats %>% 
  filter(playerId %in% c(2563033,2563132))

# Slye está nos rosters? => SIM
teams_rosters %>% 
  select(teamId, name, rosters) %>% 
  unnest(rosters) %>%  # 210 elementos 
  inner_join(players_stats, by="playerId") %>% 
  filter(playerId %in% c(2563033,2563132))

# Sly está na tabela "de-para" ffa/nfl?? => NÃO
player_ids %>% 
  filter(nfl_id %in% c(2563033,2563132))

# the lost IDs
players_stats %>% 
  anti_join(player_ids, by=c("playerId"="nfl_id")) %>% 
  filter(nflTeamAbbr!="") %>% 
  select(playerId, firstName, lastName, position, nflTeamAbbr) %>% ### <<< IDs não mapeados
  inner_join(proj_table, by=c("position", "firstName"="first_name", "lastName"="last_name")) %>% 
  select(id, nfl_id=playerId) %>% 
  saveRDS("./data/playerIds_not_mapped.rds")


  


