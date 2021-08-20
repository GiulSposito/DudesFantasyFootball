library(tidyverse)
library(ffanalytics)

# usual scrap
scrp <- scrape_data(src = c("CBS", "ESPN", "FantasyData", "FantasyPros", "FantasySharks", "FFToday",
                            "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL", "RTSports",
                            "Walterfootball", "FleaFlicker"),
                    pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                    season = 2021,
                    week = 0)

# saveRDS(scrp,"./data/season_scrap.rds")
# saveRDS(scrp,"./data/season_scrap_fleaflicker.rds")

# checking
scrp %>% 
  map_df(~select(.x, data_src, team, id), .id="pos") %>% 
  count(data_src, pos) %>% 
  pivot_wider(names_from = "pos",values_from="n")


scrp %>% 
  map_df(~select(.x, data_src, team), .id="pos") %>% 
  distinct() %>% 
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
