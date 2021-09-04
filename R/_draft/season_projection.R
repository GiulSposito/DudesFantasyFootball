library(ffanalytics)
library(yaml)

# configurations
config <- read_yaml("./config/config.yml")

##### SCRAP AND CALCULATIONS

# # First, we use a script to scrape player’s projected points from numerous
# # sources using R. 
# scrp_df <- scrape_data(
#   #src = c("CBS", "FantasyPros", "FFToday", "FantasySharks"),
#   pos = c("QB", "RB", "WR", "TE", "K", "DST"),
#   season = 2020,
#   week = 0
# )
# 
# # Second, calc projections
# proj_df <- projections_table(scrp_df, scoring_rules = read_yaml("./config/score_settings.yml"))
# 
# # add player ino
# projs <- proj_df %>% 
#   add_player_info() %>% 
#   mutate(id=as.integer(id))

# MATCHUPS
source("./R/api/ffa_league.R")
leagueMatchups <- ffa_league_matchups(config$authToken, config$leagueId, 1)
matchups_games <- ffa_extractMatchups(leagueMatchups)
teams_rosters  <- ffa_extractTeamsFromMatchups(leagueMatchups)   

#players ids
# carregando tabelas de "de para" de IDs de Jogadores
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs
my_player_ids <- player_ids %>%
  mutate(
    id = as.integer(id), 
    nfl_id = as.integer(nfl_id)) %>%
  as_tibble() 

# season projection
proj <- readRDS("./data/season_projtable.rds") %>% 
  mutate( id = as.integer(id) )

drafted_season_proj <- teams_rosters %>% 
  select(teamId, name, rosters) %>% 
  unnest(rosters) %>% 
  inner_join(select(my_player_ids, id, nfl_id), by=c("playerId"="nfl_id")) %>% 
  inner_join(proj, by = "id") %>% 
  filter(avg_type=="average") %>% 
  select(teamId, name, id, first_name, last_name, position, avg_type, floor, points, ceiling) %>% 
  group_by(teamId, name) %>% 
  summarise(
    floor = sum(floor, na.rm = T),
    points = sum(points, na.rm = T),
    ceiling = sum(ceiling, na.rm = T)
  ) %>% 
  ungroup() %>% 
  arrange( desc(points) ) 

saveRDS(drafted_season_proj, "./data/drafted_season_projection.rds")


