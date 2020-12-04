teams_rosters %>% 
  select(teamId, name, imageUrl, week.stats, season.stats) %>% 
  unnest(week.stats) %>% 
  rename(week.pts = pts ) %>% 
  unnest(season.stats) %>% 
  mutate(
    across(c(rank, divisionRank, wins, losses, ties, waiverPriority), as.integer),
    across(c(pts, ptsAgainst), as.numeric)
  ) %>% 
  rename(season.pts=pts, season.ptsAgainst=ptsAgainst) %>% 
  saveRDS(glue("./data/rank_week{week}.rds"))

