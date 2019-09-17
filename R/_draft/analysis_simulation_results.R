library(plotly)


sim.eval <- sim %>% 
  select ( home.name, home.pts, home.sim.org, away.name, away.pts, away.sim.org) %>% 
  mutate( home.win.org = map2(home.sim.org, away.sim.org, function(.h, .a){
      return(.h>.a)
  })) %>% 
  mutate( away.win.org = map(home.win.org, ~!.x)) %>% 
  mutate( home.win = home.pts>away.pts,
          away.win = !home.win,
          home.win.org.prob = map_dbl(home.win.org, mean),
          away.win.org.prob = map_dbl(away.win.org, mean),
          home.win.pred = home.win.org.prob>.5, 
          away.win.pred = away.win.org.prob>.5)

sim.eval %>% 
  mutate( game = paste0(away.name," @ ",home.name) ) %>% 
  mutate( prediction = case_when(home.win.pred==T ~ home.name,
                                 away.win.pred==T ~ away.name) ) %>% 
  mutate( who.won = case_when(home.win==T ~ home.name,
                              away.win==T ~ away.name) ) %>% 
  select(game, prediction, who.won) %>% 
  View()
  

sim.points <- sim.eval %>%
  select(home.name, home.pts, away.name, away.pts, home.sim.org, away.sim.org) %>% 
  mutate(home.pts.prd = map_dbl(home.sim.org, median),
         away.pts.prd = map_dbl(away.sim.org, median),
         home.pts.prd.min = map_dbl(home.sim.org, quantile, .05),
         away.pts.prd.min = map_dbl(away.sim.org, quantile, .05),
         home.pts.prd.max = map_dbl(home.sim.org, quantile, .95),
         away.pts.prd.max = map_dbl(away.sim.org, quantile, .95)) %>% 
  select(-home.sim.org, -away.sim.org)

sim.pts.eval <- bind_rows(
  sim.points %>% select(starts_with("home")) %>% set_names(gsub("home.", "", names(.))),
  sim.points %>% select(starts_with("away")) %>% set_names(gsub("away.", "", names(.)))
  ) %>% 
  set_names(c("team","real.points", "pred.points", "pred.min", "pred.max"))

sim.pts.eval %>% 
  ggplot(aes(x=real.points, y=pred.points, color=team)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=pred.min, ymax=pred.max), size=1) +
  geom_abline(intercept=c(0,0), slope=1, color="darkgrey", size=1, linetype="dotted") +
  ylim(70,160) +
  xlim(70,160) +
  ggtitle("Pontuação Real x Pontuacao Previsa", subtitle = "90% CI") +
  theme_minimal()

ggplotly(g)


sim.pts.eval %>% 
  select(team, real.points, pred.min, pred.points, pred.max) %>% 
  arrange(desc(real.points)) %>% 
  View()
