library(tidyverse)
library(glue)
source("./R/tidy/matchups.R")

.week <- 8

# retorna um summary como um data.frame
summaryAsTibble <- . %>% summary() %>% as.list() %>% as.tibble()

quantileAsTibble <- function(x, probs){
  quantile(x, probs, na.rm = T) %>% 
    as.list() %>% 
    as.tibble() %>% 
    purrr::set_names(c("low","med","high")) %>% 
    return()
}

# matchups and rosters (nfl)
matchups <- readRDS(glue("./data/week{.week}_matchups_json.rds")) %>% 
  extractTeams() %>% 
  mutate( 
    home.roster = map(home.roster, nfl2ffa, .ids=.players_id), # to ffa ids
    away.roster = map(away.roster, nfl2ffa, .ids=.players_id)  # to ffa ids
  )
  
# historic fantasy points (nfl)
points <- readRDS("./data/players_points.rds")
  
# players projections (ffa)
pts.proj <- readRDS("./data/points_projection.rds")

# calcula distribuição do erro
errors <- points %>%
  filter(week!=.week) %>% 
  inner_join(pts.proj, by = c("id", "season", "week")) %>% 
  select( id, position, week, season, data_src, points, pts.proj ) %>% 
  mutate( error=points-pts.proj )

# visualiza os numeros
# errors %>%
#   ggplot() +
#   geom_density(aes(error, fill=position)) +
#   facet_grid(position~data_src) +
#   theme_minimal()

# calcula distribuição de erros por posicao e source
error.dist <- errors %>% 
  select(data_src, position, error) %>% 
  nest(error) %>% 
  mutate( error.dist = map(data, function(.data) pull(.data,error)) ) %>% 
  select(-data)

# adicionar projeção e curva de probabilidade
addPlayerSimulation <- function(.team, .pts.proj, .error.dist){
  .team %>% 
    inner_join(.pts.proj, by="id") %>% 
    select(-week, -pos, -season) %>% 
    inner_join(.error.dist, by = c("position", "data_src")) %>% 
    mutate(
      pts.dist = map2(pts.proj, error.dist, function(.pts,.dst) .pts+.dst),
      pts.dist = map(pts.dist, base::sample, size=100, replace=T)
    ) %>% 
    group_by(id) %>%
    mutate( pts.proj.md = median(pts.proj, na.rm = T) ) %>% 
    group_by(id, name, position, rosterSlot, points, pts.proj.md) %>% 
    nest(pts.dist, .key="pts.dist") %>% 
    mutate( pts.range = map(pts.dist, unlist),
            pts.range.summary = map(pts.range, summaryAsTibble),
            pts.range.80pct   = map(pts.range, quantileAsTibble, probs=c(.125, .50, .875)) ) %>% 
    select(-pts.dist) %>% 
    return()
}

matchups %>% 
  mutate(
    home.roster = map(
      home.roster,
      addPlayerSimulation,
      .pts.proj = pts.proj %>% filter(week == .week),
      .error.dist = error.dist
    ),
    away.roster = map(
      away.roster,
      addPlayerSimulation,
      .pts.proj = pts.proj %>% filter(week == .week),
      .error.dist = error.dist
    )
  ) -> matchup.projs


simRosterPontuation <- function(.team, .currPoints=FALSE) {
  .team %>%
    filter(rosterSlot != "BN") %>% 
    mutate(
      player.simulation = map(
        pts.range,
        base::sample,
        size = 2000,
        replace = T
      ),
      points.made = map(points, rep, times=2000)
    ) -> resp
  
    if (.currPoints) {
      resp %>% 
        mutate(
          player.simulation = case_when(
            points == 0 ~ player.simulation,
            points != 0 ~ points.made
          )
        ) -> resp
    }

    resp %>% 
      pull(player.simulation) %>%
      bind_cols() %>%
      as.matrix() %>%
      rowSums(na.rm = T) %>%
      return()
}

matchup.projs %>% 
  mutate(
    home.sim = map(home.roster, simRosterPontuation, .currPoints=T),
    away.sim = map(away.roster, simRosterPontuation, .currPoints=T),
    home.sim.org = map(home.roster, simRosterPontuation),
    away.sim.org = map(away.roster, simRosterPontuation),
    home.win = map2(home.sim, away.sim, function(h.scr, a.scr) (h.scr > a.scr)),
    away.win = map(home.win, function(.x) (!.x) ),
    home.win.prob = map_dbl(home.win, function(.x) mean(.x)),
    away.win.prob = map_dbl(away.win, function(.x) mean(.x)),
    score.diff     = map2(home.sim, away.sim, function(h.scr, a.scr) (h.scr - a.scr)),
    score.diff.org = map2(home.sim.org, away.sim.org, function(h.scr, a.scr) (h.scr - a.scr)),
    home.points = map(home.sim, summaryAsTibble),
    away.points = map(away.sim, summaryAsTibble)
  ) -> matchup.simulation

saveRDS(matchup.simulation, glue("./data/week{.week}_simulation_v2.rds"))
