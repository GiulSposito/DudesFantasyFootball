pres <- readRDS("./data/post_matchups_results.rds")

pres %>% length()

week <- 1

mtchs <- pres[[week]]
game <- mtchs[[1]]
team <- game$leagues$matchup$homeTeam
ateam <- game$leagues$matchup$awayTeam







team$players[[1]]$rosterSlot
team$players[[1]]$fantasyPts %>% 
  mutate(col)


  select(rosterSlot, fantasyPts) %>%
  mutate(fantasyPts=list(fantasyPts)) %>% 
  unnest(fantasyPts)
  as_tibble() %>% 
  mutate(slot = ifelse(rosterSlot=="BN", "BN","ST")) %>% dim()
  purrr::set_names(c("rosterSlot","season","week","pts","slot"))
  group_by(slot) %>% 
  set_names()
  summarise(points=`$$pts`)


mtcars %>% 
  set_names(toupper(names(.))) %>% 
  glimpse()
  