library(tidyverse)

w11 <- readRDS("./data/week11_simulation_v3.rds")

w_v2 <- dir("./data", "week[0-9]+_[a-z]+_v2.*", full.names = T)
w_v3 <- dir("./data", "week[0-9]+_[a-z]+_v3.*", full.names = T)

wks_names <- c(w_v2,w_v3)

all.sims <- tibble(
  week.id = wks_names %>% str_extract("(week[0-9]+)"),
  version = wks_names %>% str_extract("(v[0-9]+)"),
  sims    = map(wks_names, readRDS)
)

all.sims %>%
  mutate(
    sim.summary = map(
      sims, 
      function(x) {
        x %>% 
        mutate(
          home.pred.pts = map_dbl(home.sim.org, median),
          away.pred.pts = map_dbl(away.sim.org, median)) %>%
        select(home.name, home.pts, home.pred.pts, away.pred.pts, away.pts, away.name)
      })
    ) %>% 
  unnest(sim.summary) %>% 
  mutate(
    win = ifelse(home.pts>away.pts,"home","away"),
    win.pred = ifelse(home.pred.pts>away.pred.pts,"home","away"),
    score.diff = away.pts-home.pts,
    score.diff.pred = away.pred.pts-home.pred.pts,
    correct.win = win==win.pred
  ) -> results

table(results$win.pred, results$win)
ggplot(results, aes(x=score.diff, y=score.diff.pred, group=week.id)) +
  geom_point(aes(color=version, shape=correct.win), size=3) +
  #stat_smooth(aes(color=version), method = lm) +
  theme_minimal()
