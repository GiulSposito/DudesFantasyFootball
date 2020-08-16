library(tidyverse)
scraps <- readRDS("./data/week2_scrap.rds")

# SCRAP
scraps[["K"]] %>% 
  filter(team=="CAR", data_src=="NumberFire") %>%
  mutate(id=as.integer(id)) %>%
  glimpse()

scraps[["K"]] %>% 
  filter(id==14600) %>% 
  glimpse()

# PLAYER ID 
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
player_ids <- player_ids %>% 
  mutate(id=as.integer(id),
         nfl_id=as.integer(nfl_id)) %>% 
  mutate(nfl_id = ifelse(id==14600,2563132, nfl_id)) %>% 
  as_tibble()

player_ids %>%
  filter(numfire_id=="robbie-gould") %>% 
  glimpse()
  
player_ids %>%
  filter(numfire_id=="joey-slye") %>% 
  glimpse()

# FANTASY
matchups <- readRDS("./data/week2_matchups_json.rds")

x <- matchups %>% 
  map(~as_tibble(.x[[1]]))

x[[1]]$matchup$awayTeam$players[[1]] %>% 
  filter(position=="K")  %>% 
  glimpse()


### Keys Sequence ###
#
# Scrap (id) -> (id) Players_id (nfl_id) -> Fantasy(id)

