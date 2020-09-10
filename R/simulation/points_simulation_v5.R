.week     <- week
.season   <- season
.ptsproj  <- ptsproj
.matchups <- matchups_games
.teams    <- teams_rosters
.plstats  <- players_stats
.plids    <- player_ids

# tipos de status que zera a pontuacao
INJURY_STATUS_ZERO <- c("Suspended","Injured Reserve","Not With Team")

mtch <- .matchups %>% 
  select(matchupId, week, awayTeam.teamId, homeTeam.teamId)

tms <- .teams %>% 
  select(teamId, teamName=name, rosters) %>% 
  unnest(rosters)

stats <- .plstats %>% 
  select(playerId, name, position, byeWeek, injuryGameStatus, weekPts) %>% 
  unnest(weekPts) %>% 
  filter(week==1) 

projs <- .ptsproj %>% 
  inner_join(select(.plids, id, playerId=nfl_id), by="id") %>% 
  filter(week==.week, season==.season) %>% 
  select(id, playerId, pts.proj) %>% 
  group_by(id, playerId) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(pts.proj = map(data, ~.x$pts.proj )) %>% 
  select(-data)

SIMULATION_SIZE = 1000

players_sim <- tms %>% 
  inner_join(projs, by="playerId") %>% 
  mutate( simulation = map(pts.proj, sample, size=SIMULATION_SIZE, replace=T))

teams_sim <- players_sim %>% 
  inner_join(stats, by="playerId") %>% 
  filter( rosterSlotId != 20 ) %>% 
  select(teamId, playerId, simulation) %>% 
  group_by(teamId) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(simulation = map(data, function(.sim){
    .sim %>% 
      pull(simulation) %>% 
      do.call(rbind, .) %>% 
      colSums() %>% return()    
  })) %>% 
  select(-data)

match_sim <- mtch %>% 
  pivot_longer(c(-matchupId, -week), names_to="name", values_to="teamId") %>% 
  mutate(name=str_remove(name, "\\.teamId")) %>% 
  inner_join(teams_sim, by="teamId") %>% 
  pivot_wider(id_cols = c(matchupId, week), names_from="name", values_from=c(teamId, simulation), names_sep=".")


match_sim %>% 
  mutate( win.homeTeam = map2(simulation.awayTeam, simulation.homeTeam, function(.sat, .sht) .sht>.sat) ) %>% 
  mutate( win.awayTeam = map(win.homeTeam, function(.wht) !.wht ) ) %>% 
  mutate( ptsdiff.homeTeam = map2(simulation.awayTeam, simulation.homeTeam, function(.sat, .sht) .sht-.sat) ) %>% 
  mutate( 
    winProb.homeTeam = map_dbl(win.homeTeam, mean), 
    winProb.awayTeam = map_dbl(win.awayTeam, mean),
    totalPts.homeTeam = map_dbl(simulation.homeTeam, median),
    totalPts.awayTeam = map_dbl(simulation.awayTeam, median)
  ) %>%
  inner_join(select(.teams, teamId, name.homeTeam=name), by=c("teamId.homeTeam"="teamId")) %>% 
  inner_join(select(.teams, teamId, name.awayTeam=name), by=c("teamId.awayTeam"="teamId")) %>% 
  select(matchupId, name.awayTeam, totalPts.awayTeam, winProb.awayTeam,
         winProb.homeTeam, totalPts.homeTeam, name.homeTeam) 
  
