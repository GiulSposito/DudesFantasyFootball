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

yahooScrap %>% 
  count(data_src,position)


#source("./R/import/scrap_nfl_fantasy_projections.R")
scrp %>% 
  addScrapTable(yahooScrap) %>% 
  map_df(~select(.x, data_src, id), .id="pos") %>% 
  count(data_src, pos) %>% 
  pivot_wider(names_from = "pos",values_from="n")

scrp %>% 
  addScrapTable(yahooScrap) %>% 
  map_df(~select(.x, data_src, team), .id="pos") %>% 
  distinct() %>% 
  count(data_src, pos) %>% 
  pivot_wider(names_from = "pos",values_from="n")


scrp$DST %>% 
  filter(data_src=="FantasyPros") %>% 
  select(data_src, id, team)

yahooScrap %>% 
  filter(position=="DST") %>% 
  arrange(desc(site_pts))

# saveRDS(scrp, "./data/season_scrap_yahoo.rds")
# 
# readRDS("./data/season_scrap.rds") %>% 
#   addScrapTable(scrp)
# yahooScrap
# 
# 
# scrp <- readRDS("./data/season_scrap.rds") 
# scrp_ff <- readRDS("./data/season_scrap_fleaflicker.rds")
# scrp_yahoo <- readRDS("./data/season_scrap_yahoo.rds")

# scrp %>% 
#   map2(scrp_ff, ~bind_rows(.x, .y)) %>% 
#   addScrapTable(scrp_yahoo) %>% 
scrp_ff %>% 
  map_df(~select(.x, data_src, id), .id="pos") %>% 
  count(data_src, pos) %>% 
  pivot_wider(names_from = "pos",values_from="n")



