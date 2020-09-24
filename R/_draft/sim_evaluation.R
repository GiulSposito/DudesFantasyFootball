library(tidyverse)
library(glue)

# PARAMETERS
.week <- 2
.version <- 5
.prefix <- "posMNF"

# recupera simulação (e resultados)
sim <- readRDS(glue("./data/simulation_v{.version}_week{.week}_posMNF.rds"))

# pontuação dos times
team_scores <- sim$teams %>% 
  select(teamId, name, week.stats, imageUrl) %>% 
  unnest(week.stats) %>% 
  filter(week==.week)

# pontuação da simulação
team_predictions <- sim$teams_sim %>% 
  mutate(
    pred.pts = map_dbl(simulation.org,median), 
    pred.90  = map_dbl(simulation.org, quantile, .9),
    pred.10  = map_dbl(simulation.org, quantile, .1),
  )

# real x previsto
scoreTable <- team_scores %>% 
  inner_join(team_predictions, by = "teamId") %>% 
  mutate(nickname=str_remove(name, "[a-zA-Z]+ ?"))


sim$matchups %>%
  inner_join( select(sim$matchup_sim, matchupId, awayTeam.winProb.org, homeTeam.winProb.org), by="matchupId")  %>% 
  select( away.teamId=awayTeam.teamId, home.teamId=homeTeam.teamId, 
          away.win = awayTeam.outcome, home.win = homeTeam.outcome,
          away.winProb.org = awayTeam.winProb.org, home.winProb.org = homeTeam.winProb.org) %>% 
  inner_join( set_names(scoreTable, paste0("away.",names(scoreTable))), by="away.teamId" ) %>% 
  inner_join( set_names(scoreTable, paste0("home.",names(scoreTable))), by="home.teamId" ) %>% 
  mutate(game.nickname=glue("{away.nickname} @ {home.nickname}")) %>%
  mutate( predicion = if_else(away.pred.pts>home.pred.pts, away.nickname, home.nickname),
          outcome   = if_else(away.win=="win", away.nickname, home.nickname),
          win.prob  = if_else(away.pred.pts>home.pred.pts, paste0(round(100*away.winProb.org,1), "%"), paste0(round(100*home.winProb.org,1),"%"))) %>% 
  select( game.nickname, predicion, win.prob, outcome )
  
playerProjs <- sim$players_id %>% 
  select(id, playerId=nfl_id) %>% 
  inner_join(sim$ptsproj, by = "id") %>% 
  filter(week==.week) %>% 
  select(playerId, data_src, proj=pts.proj)

playerPoints <- sim$players_stats %>%
  unnest(weekPts) %>% 
  filter(week==.week, !is.na(weekPts)) %>% 
  select(playerId, position, pts=weekPts) 

projErrors <- playerPoints %>% 
  inner_join(playerProjs, by="playerId") %>% 
  mutate(error=pts-proj)

  
projErrors %>% 
  ggplot() +
  geom_point(aes(x=proj, y=pts, color=position)) +
  facet_grid(position~data_src, scales = "free") +
  geom_abline(intercept=0, slope=1, linetype="dashed") + 
  labs(title=glue("Projection x Points Made"), subtitle = "By Position and Source Site",
          x="projection", y="points") +
  xlim(0,30) + ylim(0,30) +
  theme_minimal()

projErrors %>% 
  ggplot() +
  geom_density(aes(x=error, fill=position)) +
  facet_grid(position~data_src, scales = "free") +
  geom_vline(xintercept = 0, color="black", linetype="dashed") +
  ggtitle("Error Distribution", subtitle = "By Position and Source Site") +
  theme_minimal()

