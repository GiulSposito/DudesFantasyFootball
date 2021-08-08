library(rvest)
library(glue)
library(tidyverse)

page <- read_html("./import/draft_round_05.txt")

playerIds <- page %>% 
  html_nodes("a.playerCard") %>% 
  html_attr("href") %>% 
  str_extract("\\b(\\d+)$") %>% 
  unique() %>% 
  as.integer()

teamIds <- page %>% 
  html_nodes("a.teamName") %>% 
  html_attr("href") %>% 
  str_extract("\\b(\\d+)$") %>% 
  as.integer()

picks <- tibble(
  teamId   = teamIds,
  playerId = playerIds
) %>% 
  mutate( pick = row_number() )

players <- readRDS("./data/simulation_v5_week16_final.rds") %>% 
  .[["players_stats"]]

teams <- readRDS("./data/simulation_v5_week1_final.rds") %>% 
  .[["teams"]] %>% 
  mutate( nickname=str_extract(name, "\\b(\\w+)$") ) %>% 
  select(teamId, name=nickname, imageUrl)
  

players %>% 
  select(playerId, lastName, position, smallImageUrl, weekPts) %>% 
  unnest(weekPts) %>% 
  filter(week==16) %>% 
  rename(points = weekSeasonPts) %>% 
  inner_join(picks, by="playerId") %>% 
  inner_join(teams, by="teamId") %>% 
  mutate( player=glue("{lastName} ({position})")) %>% 
  ggplot(aes(x=pick, y=points, fill=points)) + 
  geom_bar(stat="identity", width = .5) +
  geom_image(aes(x=pick, y=0, image=imageUrl), nudge_y=-10) +
  geom_image(aes(x=pick, y=points, image=smallImageUrl), nudge_y=30) +
  geom_text(aes(x=pick, y=points, label=player), nudge_y=10, size=3) +
  ylim(c(-10,400)) +
  labs(title="5th Round Picks") +
  theme_classic() +
  theme( legend.position = "none" )
