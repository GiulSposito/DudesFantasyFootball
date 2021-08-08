library(tidyverse)
library(glue)

calc_expected_score <- function(team_rating, opp_team_rating) {
  return(1 / (1 + 10^((opp_team_rating - team_rating) / 400)))
}

calc_new_rating <- function(team_rating, observed_score, expected_score, k_factor = 20) {
  return(team_rating + k_factor * (observed_score - expected_score))
}

### DATA SET PREPARATION

sim <- readRDS("./data/simulation_v5_week1_final.rds")

teams <- sim$teams %>% 
  select(teamId, name) %>% 
  mutate(nickname=str_trim(str_extract(name, "\\b(\\w+)$"))) %>% 
  mutate(nickname=janitor::make_clean_names(nickname)) %>% 
  select(id=teamId, name=nickname)

h2h <- 1:14 %>% 
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

matchups <- h2h %>% 
  inner_join(teams, by=c("away.id"="id")) %>% 
  rename(away.nm=name) %>% 
  inner_join(teams, by=c("home.id"="id")) %>% 
  rename(home.nm=name) %>% 
  mutate( home.win = if_else(home.win=="win",1L,0L),
          away.win = if_else(away.win=="win",1L,0L),
          home.los = if_else(home.win==1L,0L,1L),
          away.los = if_else(away.win==1L,0L,1L))

a <- bind_cols(
  matchups %>% 
    select(week)
  ,
  matchups %>% 
    select(starts_with("home")) %>% 
    set_names(str_replace(names(.), "home","team"))
  ,
  matchups %>% 
    select(starts_with("away")) %>% 
    set_names(str_replace(names(.), "away","opp"))
)

b <- bind_cols(
  matchups %>% 
    select(week)
  ,
  matchups %>% 
    select(starts_with("away")) %>% 
    set_names(str_replace(names(.), "away","team"))
  ,
  matchups %>% 
    select(starts_with("home")) %>% 
    set_names(str_replace(names(.), "home","opp"))
)

# FULL RESULT DATASET
team.hist <- bind_rows(a,b) %>% 
  mutate( team.elo = 1500, 
          opp.elo  = 1500)


for(.week in 1:12){
  
  # recalcula elo da semana
  week.elo <- team.hist %>% 
    filter(week==.week) %>% 
    mutate(
      team.elo = calc_new_rating(
        team.elo,
        team.win,
        calc_expected_score(team.elo, opp.elo)))
  
  # propaga para as próximas
  recalc.elo <- team.hist %>% 
    filter(week>.week) %>% 
    select(-team.elo, -opp.elo) %>% 
    left_join(select(week.elo, team.id, team.elo), by=c("team.id")) %>% 
    left_join(select(week.elo, opp.id,  opp.elo), by=c("opp.id"))
  
  # junta o histórico, com a semana calculada e o propagado
  team.hist <- bind_rows(
    filter(team.hist, week<.week),
    week.elo,
    recalc.elo
  )
  
}

team.hist %>% 
  filter(week==12) %>% 
  select(team.nm, team.elo) %>% 
  arrange(desc(team.elo))

bind_rows(
  select(team.hist, week, team.nm, team.win, team.pts, team.elo),
  select(team.hist, week, team.nm=opp.nm, team.win=opp.win, team.pts=opp.pts, team.elo=opp.elo)
) %>% 
  filter(week>1) %>% 
  mutate(team.win = (team.win==1)) %>% 
  ggplot(aes(x=team.pts, y=team.elo)) +
  geom_point(aes(color=team.win), size=2) +
  geom_hline(yintercept = 1500, color="grey", linetype="dashed") +
  stat_smooth(method = "lm", se = F) +
  theme_minimal() +
  theme(legend.position = "none")

z <- team.hist %>% 
  mutate( across(c(team.elo, opp.elo), lag, 14)) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    elo.delta = team.elo-opp.elo,
    pts.delta = team.pts-opp.pts,
    pr.a=1/(10^(-elo.delta/400)+1)
  ) %>% 
  mutate(
    outcome = team.win==1, #if_else (team.win==1, "home.team","away.team"),
    prediction = pr.a>.5
    # prediction = case_when(
    #   pr.a >  .5  ~ "home.team",
    #   pr.a == .5 ~ "undefined", 
    #   pr.a <= .5 ~ "away.team")
  )

z %>% 
  select(prediction, outcome) %>% 
  mutate(across(c(prediction, outcome), factor, levels = c(T,F), labels = c("home win", "away win"))) %>% 
  caret::confusionMatrix(.$prediction, .$outcome)

  
  ggplot(aes(pr.a)) +
  geom_density(aes(color=(team.win==1))) +
  theme_minimal()



z <- team.hist %>% 
  mutate( elo.delta = team.elo-opp.elo,
          outcome = if_else (team.win==1, "home.team","away.team"),
          prediction = case_when(
            elo.delta > 0  ~ "home.team",
            elo.delta ==0 ~ "undefined", 
            elo.delta < 0 ~ "away.team"
          )) %>% 
  mutate(across(c(prediction,outcome), as.factor)) %>% 
  select(week, prediction, outcome)



