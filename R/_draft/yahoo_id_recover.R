## IDS do FFA
# carregando tabelas de "de para" de IDs de Jogadores
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!

# inputs
player_ids
scrp_df

# # defense ids
# player_ids %>% 
#   filter(id %in% formatC(0501:0532, width = "4", flag = "0")) %>% 
#   select(id, stats_id) %>% 
#   write_csv("./export/player_ids_defense.csv")
# 
# scrp_df %>% 
#   filter(position=="DEF") %>% 
#   select(src_id, player, position, team) %>% 
#   write_csv("./export/yahoo_scrp_defense.csv")
# 
# read_csv("./export/yahoo_scrp_defense.csv") %>% 
#   inner_join(player_ids)

# remove defesas (foram mapeadas na mão)
player_ids_ndef <- player_ids %>% 
  filter(!id %in% formatC(0501:0532, width = "4", flag = "0"))

yahoo_ids <- scrp %>% 
  filter(position!="DEF") %>% 
  select(data_src, src_id, player, position, team)

# qual yahoo scrap não tem equivalente no stats_id
yahoo_missing <- yahoo_ids %>% 
  anti_join(select(player_ids_ndef, id, stats_id, numfire_id), by=c("src_id"="stats_id"))
  
# tenta reconstruir uma chave baseada no nome para bater com o nome do jogador
yahoo_mis_key <- yahoo_missing %>% 
  mutate( yahoo_key = str_replace_all(str_to_lower(player)," ","_")) %>% 
  mutate(yahoo_key=str_remove_all(yahoo_key, "_jr."))

# tenta cruzar as chaves
myplayer_ids <- player_ids_ndef %>%
  select(id, stats_id, numfire_id) %>% 
  add_player_info() %>% 
  select(id, stats_id, numfire_id, first_name, last_name, team, position) %>% 
  mutate(yahoo_key = str_replace_all(str_to_lower(str_c(first_name," ",last_name))," ","_"))

# parte do dataset que bate com o player id (via stats_id)
yahoo_matched <- yahoo_ids %>% 
  inner_join(player_ids_ndef, by=c("src_id"="stats_id")) %>% 
  select(id, yahoo_id=src_id) 

# part do dataset que bate com a nova chave criada pelo nome
yahoo_not_matched <- yahoo_mis_key %>% 
  inner_join(myplayer_ids, by=c("yahoo_key")) %>% 
  select(id, yahoo_id=src_id)

# part do datase de defesas (criada na mão)
yahoo_def <- read_csv("./export/yahoo_scrp_defense.csv") %>% 
  mutate( yahoo_id = str_c(src_id,team,sep="-")) %>% 
  select(id, yahoo_id)

# unifica datasets
yahoo_map <- bind_rows(yahoo_def, yahoo_matched, yahoo_not_matched) %>% 
  distinct()

saveRDS(yahoo_map,"./data/yahoo_id_map.rds")
