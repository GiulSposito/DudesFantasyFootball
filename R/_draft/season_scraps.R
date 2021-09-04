library(tidyverse)
library(ffanalytics)

# usual scrap
scrp <- scrape_data(src = c("ESPN", "FantasyData", "FantasyPros", "FantasySharks", "FFToday", # "CBS", 
                            "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL", "RTSports", # "FleaFlicker"
                            "Walterfootball", "FleaFlicker"),
                    pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                    season = 2021,
                    week = 0)


scrp <- scrape_data(pos = c("QB", "RB", "WR", "TE", "K", "DST"),season = 2021,week = 0)


# checking
scrp %>% 
  map_df(~select(.x, data_src, team, id), .id="pos") %>% 
  count(data_src, pos) %>% 
  pivot_wider(names_from = "pos",values_from="n")


# yahoo
library(yaml)
config <- read_yaml("./config/config.yml")
source("./R/import/scrap_yahoo_fantasy_projection.R")
yahooScrap <- scrapYahooProjection(0, config$yahooCookies)

yScrp <- yahooScrap %>% 
  separate_rows(position, sep=",")

source("./R/import/scrap_nfl_fantasy_projections.R")

scrp %>% 
  addScrapTable(yScrp) %>% 
  map_df(~select(.x, data_src, id), .id="pos") %>% 
  count(data_src, pos) %>% 
  pivot_wider(names_from = "pos",values_from="n")

proj <- scrp %>% 
  addScrapTable(yScrp) %>% 
  projections_table(scoring_rules = read_yaml("./config/score_settings.yml"))

season_proj <- proj %>% 
  add_player_info() %>% 
  add_ecr() %>%
  add_risk() %>%
  add_adp() %>% 
  add_aav()

scrp %>% 
  addScrapTable(yScrp) %>% 
  saveRDS("./data/season_scrap.rds")

season_proj %>% 
  saveRDS("./data/season_projtable.rds")

season_proj %>% 
  filter(avg_type=="average") %>% 
  arrange(desc(floor_vor)) %>% 
  View()

season_proj_old <- readRDS("./data/season_projtable_20210816.rds")

proj_old <- season_proj_old %>% 
  select(id, avg_type, points_old = points)

season_proj %>% 
  inner_join(proj_old, by = c("id", "avg_type")) %>% 
  filter( position=="DST" ) %>% 
  mutate(points_diff = points - points_old) %>% 
  filter(points_diff != 0,
         avg_type == "average") %>% 
  arrange(desc(points_diff)) %>% 
  select(first_name, last_name, position, team, pts_lastweek= points_old, pts_now=points, diff=points_diff)
