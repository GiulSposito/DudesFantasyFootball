library(tidyverse)

# quem são os jogadores que performaram acima das projecoes?
sim <- readRDS("./data/simulation_v5_week5_final.rds")

# pontos feitos
week_pts <-sim$players_stats %>% 
  select(playerId, name, position, weekPts) %>% 
  unnest(weekPts)

# projecoes
projs <- sim$ptsproj %>% 
  inner_join(select(sim$players_id, id, playerId=nfl_id), by="id") %>% 
  mutate(type=case_when(
    data_src=="ceiling" ~ "limits",
    data_src=="floor"   ~ "limits",
    str_detect(data_src, "ERROR") ~ "error",
    T ~ "site"
  ))

# curious players
sel_players <- c(2558125, 2557991, 2540258,2553439, 2543498, 2541785)

# visualizing prediction againt points
week_pts %>% 
  filter( playerId %in% sel_players) %>% 
  inner_join(projs, by = c("playerId", "week")) %>% 
  ggplot(aes(pts.proj, weekPts)) +
  geom_point(aes(color=type)) +
  geom_abline(slope = 1, intercept = 0, color="grey", linetype="dashed") +
  facet_wrap(position~.) +
  theme_minimal() +
  theme(legend.position = "none")


proj_interval <- projs %>% 
  group_by(week, playerId) %>% 
  nest() %>% 
  mutate(stats=map(data, function(.x){
    .x$pts.proj %>% 
      Rmisc::CI(ci=.95) %>% 
      as.list() %>% 
      as_tibble() %>% 
      return()
  })) %>% 
  unnest(stats) %>% 
  ungroup() %>% 
  select(-data)

proj_interval %>% 
  inner_join(week_pts, by = c("playerId", "week")) %>% 
  filter(playerId %in% sel_players) %>% 
  ggplot(aes(x=weekPts, y=mean, color=position)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  geom_abline(slope = 1, color="grey", linetype="dashed") + 
  facet_wrap(week~.) +
  theme_minimal()

proj_interval %>% 
  mutate(distance=upper-lower) %>% 
  inner_join(week_pts, by = c("playerId", "week")) %>% 
  group_by(week, position) %>% 
  summarise(avg.dist=mean(distance, na.rm = T)) %>% 
  arrange(position, week) %>% View()


iris %>% 
  group_by(Species) %>% 
  dplyr::summarise( sp.len = mean(Sepal.Length))


projs %>% 
  group_by(week, playerId) %>% 
  summarise(stats=Rmisc::CI(pts.proj))
