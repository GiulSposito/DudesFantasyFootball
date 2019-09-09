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
  select(home.name, home.win, home.win.pred, away.win.pred, away.win, away.name)


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

g <- sim.pts.eval %>% 
  ggplot(aes(x=real.points, y=pred.points, color=team)) +
  geom_point(size=1) +
  geom_errorbar(aes(ymin=pred.min, ymax=pred.max)) +
  geom_abline(intercept=c(0,0), slope=1, color="grey", linetype="dotted") +
  ylim(0,150) +
  xlim(0,150) +
  theme_minimal()

ggplotly(g)
