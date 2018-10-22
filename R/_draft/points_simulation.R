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
  mutate( home.roster = map(
    home.roster,
    addPlayerSimulation,
    .pts.proj = pts.proj.week,
    .error.dist = error.dist
  ))


addPlayerSimulation(.team, pts.proj.week, error.dist) %>% 
  filter(id==13614) %>% 
  pull(pts.dist.array) %>% unlist() %>%  hist(breaks=50)


