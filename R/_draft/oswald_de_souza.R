library(tidyverse)
library(lubridate)
library(flexdashboard)
library(glue)

source("./R/import/checkFantasyAPI.R")
checkFantasyAPI(1)

source("./R/import/checkFantasyAPI.R")
checkFantasyAPI(week)

# carregando tabelas de "de para" correcao do ID de jogadores
load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
player_ids <- player_ids %>% 
  mutate( 
    id     = as.integer(id), 
    nfl_id = as.integer(nfl_id)) %>%
  # joey slye fixies
  mutate(
    nfl_id        = ifelse(id==14600, 2563132,     nfl_id),
    fantasypro_id = ifelse(id==14600, "joey-slye", fantasypro_id),
    fftoday_id    = ifelse(id==14600, "16763",     fftoday_id),
  ) %>%  
  as_tibble()

source("./R/import/import_matchups.R")
source("./R/tidy/matchups.R")
matchups <- 11:14 %>% 
  map(~importMatchups(.x)) %>% 
  map_df(extractTeams)

rank <- matchups %>% 
  filter(week==11) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x){
    tibble(
      team = c(.x$away.name, .x$home.name),
      record = c(.x$away.record, .x$home.record)
    )
  }) %>% 
  separate(record,c("win", "loss"), sep = "-",convert = T, extra = "drop") %>% 
  as_tibble()
  

round <- matchups %>% 
  select (week, away.name, home.name) %>% 
  filter(week==14) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x){
    tibble(
      team = c(.x$away.name, .x$home.name),
      win  = c(0,1),
      loss = c(1,0)
    )
  }) 

rank <- round %>% 
  gather(key="result",value="value", -team) %>% 
  select(-value) %>% 
  inner_join(rank, by="team") %>% 
  arrange(team) %>% 
  mutate( win=ifelse(result=="win", win+1, win),
          loss=ifelse(result=="loss", loss+1, loss))


  
rank <- select(rank, -result)

rank %>% 
  mutate(record=paste0(win,"-",loss)) %>% 
  count(team, record) %>% 
  group_by(team) %>% 
  mutate(
    total=sum(n), 
    pct = n/total
  ) %>% 
  ungroup() %>% 
  spread(record, pct) %>% 
  View()

