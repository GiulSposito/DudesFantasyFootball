library(rvest)
library(stringr)


read_html("./data/draft_recap.html") %>% 
  html_nodes("a.playerName") %>% 
  html_text() -> playerNames

read_html("./data/draft_recap.html") %>% 
  html_nodes("a.playerName") %>% 
  html_attr("href") %>% 
  str_extract("playerId=.*") %>% 
  str_replace("playerId=","") %>% 
  as.integer() -> playerIds

read_html("./data/draft_recap.html") %>% 
  html_nodes("a.teamName") %>% 
  html_text() -> teamNames

read_html("./data/draft_recap.html") %>% 
  html_nodes("a.teamName") %>% 
  html_attr("href") %>% 
  str_replace("/league/3940933/team/","") %>% 
  as.integer() -> teamIds

draft <- tibble(
  round       = rep(1:8,each=15),
  pick        = 1:120,
  player.id   = playerIds,
  player.name = playerNames,
  team.id     = teamIds,
  team.name   = teamNames
)

draft

