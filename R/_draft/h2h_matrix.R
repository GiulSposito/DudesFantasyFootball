library(tidyverse)
library(glue)

### DATA SET PREPARATION

sim <- readRDS("./data/simulation_v5_week1_final.rds")

teams <- sim$teams %>% 
  select(teamId, name) %>% 
  mutate(nickname=str_trim(str_extract(name, "\\b(\\w+)$"))) %>% 
  mutate(nickname=janitor::make_clean_names(nickname)) %>% 
  select(id=teamId, name=nickname)

h2h <- 1:12 %>% 
  map_df(function(.w){
    
    sim <- glue("./data/simulation_v5_week{.w}_final.rds") %T>% 
      print() %>% 
      readRDS()
    
    match_result <- sim$matchups %>% 
      select(week, awayTeam.teamId, awayTeam.outcome, homeTeam.teamId, homeTeam.outcome) %>% 
      set_names(c("week", "away.id","away.win","home.id","home.win"))
    
    match_points <- sim$matchup_sim %>%
      select(week, homeTeam.teamId, awayTeam.teamId, homeTeam.totalPts, awayTeam.totalPts) %>% 
      set_names(c("week","home.id","away.id", "home.pts", "away.pts"))
    
    inner_join(match_result, match_points, by = c("week", "away.id", "home.id"))
  }) 

h2h_raw <- h2h %>% 
  inner_join(teams, by=c("away.id"="id")) %>% 
  rename(away.nm=name) %>% 
  inner_join(teams, by=c("home.id"="id")) %>% 
  rename(home.nm=name) %>% 
  mutate( home.win = if_else(home.win=="win",1L,0L),
          away.win = if_else(away.win=="win",1L,0L),
          home.los = if_else(home.win==1L,0L,1L),
          away.los = if_else(away.win==1L,0L,1L))

a <- bind_cols(
  h2h_raw %>% 
    select(week)
  ,
  h2h_raw %>% 
    select(starts_with("home")) %>% 
    set_names(str_replace(names(.), "home","team"))
  ,
  h2h_raw %>% 
    select(starts_with("away")) %>% 
    set_names(str_replace(names(.), "away","opp"))
)

b <- bind_cols(
  h2h_raw %>% 
    select(week)
  ,
  h2h_raw %>% 
    select(starts_with("away")) %>% 
    set_names(str_replace(names(.), "away","team"))
  ,
  h2h_raw %>% 
    select(starts_with("home")) %>% 
    set_names(str_replace(names(.), "home","opp"))
)

# FULL RESULT DATASET
h2h_full <- bind_rows(a,b)

#### VICTORY WITHIN GROUP TIE BRAKE FUNCTION

h2hRank <- function(ids, h2h_data){
  h2h_data %>% 
    filter( (team.id %in% ids) & (opp.id %in% ids) ) %>% 
    group_by(team.id, team.nm) %>% 
    summarise (
      W = sum(team.win),
      L = sum(team.los),
      pts.pro = sum(team.pts),
      pts.agt = sum(opp.pts),
      pct = W/(W+L)
    ) %>% 
    ungroup() %>% 
    mutate( perf = paste0(W,"-",L) ) %>% 
    arrange(desc(pct)) %>% 
    mutate( rank = rank(-pct, ties.method = "min")-1 ) %>% 
    add_count(rank) %>% 
    select(team.id, perf, pct, rank, n) %>% 
    return()
}

#### BUILDING THE GLOBAL RANK

h2h_rank <- h2h_full %>% 
  arrange(week, team.id) %>% 
  group_by(team.id, team.nm) %>% 
  summarise (
    W = sum(team.win),
    L = sum(team.los),
    pts.pro = sum(team.pts),
    pts.agt = sum(opp.pts),
    pct = W/(W+L)
  ) %>% 
  ungroup() %>% 
  mutate( perf = paste0(W,"-",L) ) %>% 
  arrange(desc(pct)) %>% 
  mutate( rank = rank(-pct, ties.method = "min") ) %>% 
  add_count(rank)

h2h_rank

#### REPEAT UNTIL THE CLASSIFICATION DO NO CHANGE ANYMORE

# BUILD A TIE BREAKING RANK (VICTORY WITHIN GROUP)
t1_rank <- h2h_rank %>% 
  filter(n>1) %>% 
  group_by(rank, n) %>% 
  nest() %>% 
  mutate(
    t1 = map(data, function(.x){
      .x %>% 
        select(team.id) %>% 
        pull() %>% 
        h2hRank(h2h_full)
    }) 
  ) %>% 
  ungroup() %>% 
  select(t1) %>% 
  unnest(t1, names_sep=".") %>% 
  rename(team.id=t1.team.id)

# UPDATE GLOBAL RANK WITH VICTORY GROUP TIE BREAKING
h2h_rank <- h2h_rank %>% 
  left_join(t1_rank, by="team.id") %>% 
  mutate( t1.rank = ifelse(is.na(t1.rank), 0, t1.rank)) %>% 
  arrange(rank, t1.rank) %>% 
  mutate( rank = rank + t1.rank ) %>% 
  select( -starts_with("t1."), -n ) %>% 
  add_count( rank )

h2h_rank

##### IF THE RANK DON'T CHANGE AND IF WE HAVE A TIE USES POINTS FOR AND AGAINT

# BUILD A RANK FOR POINTS
t2_rank <- h2h_rank %>% 
  filter(n>1) %>% 
  group_by(rank) %>% 
  nest() %>% 
  mutate(t2 = map(data, function(.x){
    .x %>% 
      mutate( rank=rank(-pts.pro, pts.agt)-1 ) %>% 
      select(team.id, rank) %>% 
      arrange(rank) %>% 
      return()
  })) %>% 
  ungroup() %>% 
  select(t2) %>% 
  unnest(t2, names_sep=".") %>% 
  rename(team.id=t2.team.id)

# UPDATE GLOBAL RANK
h2h_rank %>% 
  left_join(t2_rank) %>% 
  mutate( t2.rank = if_else(is.na(t2.rank),0,t2.rank)) %>% 
  mutate( rank = rank+t2.rank ) %>% 
  select(-t2.rank, -n) %>% 
  add_count(rank) %>% 
  arrange(rank)

h2h_rank
