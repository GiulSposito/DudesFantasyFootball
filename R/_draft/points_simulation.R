library(tidyverse)
source("./R/tidy/matchups.R")

# translatig NFL ID to FFA ID
nfl2ffa <- function(.dtf, .ids) {
  .ids %>% 
    select(id, src_id) %>% 
    inner_join(.dtf, by="src_id") %>% 
    select(-src_id) %>% 
    return()
}

# retorna um summary como um data.frame
summaryAsTibble <- . %>% summary() %>% as.list() %>% as.tibble()

# mapping src_id (nfl) <-> id (ffa)
players_id <- readRDS("./data/nfl_players_id.rds")

# matchups and rosters (nfl)
matchups <- readRDS("./data/week7_matchups_json.rds") %>% 
  extractTeams() %>% 
  mutate( 
    home.roster = map(home.roster, nfl2ffa, .ids=players_id), # to ffa ids
    away.roster = map(away.roster, nfl2ffa, .ids=players_id)  # to ffa ids
  )
  
# historic fantasy points (nfl)
points <- readRDS("./data/players_points.rds") %>% 
  nfl2ffa(.ids=players_id) # to ffa ids
  
# players projections (ffa)
pts.proj <- readRDS("./data/points_projection.rds")

# calcula distribuição do erro
errors <- points %>%
  filter(week!=7) %>% 
  inner_join(pts.proj, by = c("id", "season", "week")) %>% 
  select( id, position, week, season, data_src, points, pts.proj ) %>% 
  mutate( error=points-pts.proj )

# visualiza os numeros
errors %>% 
  ggplot() +
  geom_density(aes(error, fill=position)) +
  facet_grid(position~data_src) +
  theme_minimal()

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
    mutate( pts.dist.array = map(pts.dist, unlist) ) %>% 
    select(-pts.dist) %>% 
    return()
}

matchups %>% 
  mutate(
    home.roster = map(
      home.roster,
      addPlayerSimulation,
      .pts.proj = pts.proj %>% filter(week == 7),
      .error.dist = error.dist
    ),
    away.roster = map(
      away.roster,
      addPlayerSimulation,
      .pts.proj = pts.proj %>% filter(week == 7),
      .error.dist = error.dist
    )
  ) -> matchup.projs


simRosterPontuation <- function(.team) {
  .team %>%
    filter(rosterSlot != "BN") %>%
    mutate(player.simulation = map(
      pts.dist.array,
      base::sample,
      size = 2000,
      replace = T
    )) %>%
    pull(player.simulation) %>%
    bind_cols() %>%
    as.matrix() %>%
    rowSums(na.rm = T) %>%
    return()
}

matchup.projs %>% 
  mutate(
    home.sim = map(home.roster, simRosterPontuation),
    away.sim = map(away.roster, simRosterPontuation),
    home.win = map2(home.sim, away.sim, function(h.scr, a.scr) (h.scr > a.scr)),
    away.win = map(home.win, function(.x) !.x),
    home.win.prob = map_dbl(home.win, function(.x) mean(.x)),
    away.win.prob = map_dbl(away.win, function(.x) mean(.x)),
    score.diff = map2(home.sim, away.sim, function(h.scr, a.scr) (h.scr - a.scr)),
    home.points = map(home.sim, summaryAsTibble),
    away.points = map(away.sim, summaryAsTibble)
  ) -> matchup.simulation

View(matchup.simulation)
