matchups_games
teams_rosters %>%
  select(teamId, name, stats, week.stats, season.stats) %>% 
  unnest(week.stats)


sim_summary %>% names()
  select(teamId, teamName=name, winProb, imageUrl, )
  
games_summary %>% 
  select(game.nickname, away.nickname, away.projPts, away.pts, home.pts, home.projPts, home.nickname)
