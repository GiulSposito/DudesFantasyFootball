library(knitr)
library(markdown)
library(flexdashboard)
library(lubridate)
library(glue)
library(tidyverse)
source("./R/import/checkFantasyAPI.R")
source("./R/import/import_matchups.R")
source("./R/simulation/points_simulation_v3.R")

# import matchups
importMatchups(1) -> matchups

pstats <- readRDS("./data/players_points.rds")

head(pstats)

# matchups and rosters (nfl)
source("./R/tidy/matchups.R")
teams <- matchups %>% 
  extractTeams() %>% 
  mutate( 
    home.roster = map(home.roster, nfl2ffa, .ids=.players_id), # to ffa ids
    away.roster = map(away.roster, nfl2ffa, .ids=.players_id)  # to ffa ids
  )

# tydi teams
c("home","away") %>% 
  map(
    function(.prefix,.teams) {
      .teams %>% 
        select(starts_with(.prefix)) %>% 
        set_names(gsub(pattern=paste0(.prefix,"\\."),replacement = "",x=names(.))) %>%
        select(-pts) %>%
        rename(teamName=name) %>% 
        return()
    },
    .teams=teams
  ) %>% 
  bind_rows() %>% 
  unnest(roster)  -> players.team

players.team
pstats 
draft 

draft %>%
  rename(id=player.id, teamId=team.id, teamName=team.name) %>% 
  inner_join(pstats, by=c("id"="src_id")) -> pstats.team

pstats.team %>% 
  group_by(teamId, teamName) %>% 
  summarise( season.pts = sum(points) ) %>% 
  select(teamName, season.pts ) %>% 
  arrange(desc(season.pts))

pstats.team %>% 
  group_by(teamId, teamName, week) %>% 
  summarise( week.pts = sum(points) ) %>% 
  ggplot( aes(x=week, y=week.pts, color=teamName) ) +
  geom_line(size=2) +
  geom_label(aes(label=week.pts), size=4, label.padding = unit(2, "points"), show.legend = F) +
  theme_minimal() +
  theme( legend.position = "bottom" )

pstats.team %>% 
  group_by(round, pick, teamName, id, player.name, position) %>% 
  summarise( season.pts = sum(points) ) %>% 
  ungroup() %>% 
  filter( position %in% c("WR","RB") ) %>% 
  arrange( desc(season.pts) ) %>% 
  mutate( rank = 1:nrow(.)) %>% 
  arrange( pick ) %>% 
  View()


pstats.team %>% 
  group_by(round, pick, teamName, id, player.name, position) %>% 
  summarise( season.pts = sum(points) ) %>% 
  ungroup() %>% 
  filter( position %in% c("WR","RB") ) %>% 
  arrange( desc(season.pts) ) %>% 
  mutate( rank = 1:nrow(.) ) %>% 
  mutate( grade = pick-rank ) %>% 
  View()


pstats %>% 
  group_by(id, src_id, firstName, lastName, position ) %>% 
  summarise( season.pts=sum(points) ) %>% 
  mutate( full.name = paste0(firstName, " ", lastName) ) %>% 
  ungroup() %>% 
  select(2,7,5,6) %>% 
  rename( id=src_id)-> all.players

allpstats <- left_join(all.players, draft %>% rename(id=player.id, teamId=team.id, teamName=team.name))

allpstats %>% 
  #filter( position %in% c("WR","RB") ) %>% 
  arrange( desc(season.pts) ) %>% 
  mutate( rank = 1:nrow(.) ) %>% 
  mutate( grade = pick-rank ) %>% 
  select( -player.name ) %>% 
  filter( !is.na(teamId) ) %>% 
  group_by(teamId, teamName) %>% 
  summarise( grade = sum(grade, na.rm = T)) %>% 
  arrange(desc(grade))



allpstats %>% filter(!is.na(teamName))
