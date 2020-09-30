
proj_table2 <- projections_table(scraps, read_yaml("./config/score_settings.yml"))

prj2 <- proj_table2 %>% 
  add_player_info()


prj2 %>% 
  select(id, pos, avg_type, points, floor, ceiling, sd_pts)


sim <- readRDS("./data/simulation_v5_week3_posMNF.rds")


pr1 <- sim$proj_table %>% 
  select(id, floor, points, ceiling) %>% 
  arrange(desc(points))


pr2 <- sim$ptsproj %>% 
  filter(week==3) %>% 
  group_by(id) %>% 
  summarise(
    floor = quantile(pts.proj, .05),
    points = median(pts.proj),
    ceiling = quantile(pts.proj, .95)
  ) %>% 
  arrange(desc(points))

st <- sim$players_stats %>% 
  select(playerId, position, weekPts) %>% 
  unnest(weekPts) %>% 
  filter(week==3) %>% 
  inner_join(select(sim$players_id, id, playerId=nfl_id), by="playerId") %>% 
  select(-weekSeasonPts, -playerId) %>%  
  arrange(desc(weekPts))

st %>% 
  inner_join(pr2) %>% 
  mutate(inrange=(weekPts>=floor & weekPts<=ceiling)) %>% 
  group_by(position) %>% 
  summarise(inRangePct = mean(inrange, na.rm=T))

st %>% 
  inner_join(pr1) %>% 
  mutate(se=(weekPts-points)^2) %>% 
  group_by(position) %>% 
  summarise(
    mse=mean(se, na.rm=T),
    rmse=sqrt(mse)
  )

data.model <- sim$ptsproj %>% 
  filter(week==3) %>% 
  inner_join(st) %>% 
  group_by(id, data_src) %>% 
  summarise (
    pts.proj = mean(pts.proj), 
    weekPts  = mean(weekPts)
  ) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(id, weekPts), names_from=data_src, values_from=pts.proj) %>% 
  select(-id) 

data.model.in <- mice(data.model, m=5, method="pmm", maxit = 10, seed=42)

data.model.ok <- complete(data.model.in) %>% as_tibble()

m <- lm(weekPts~. , data.model.ok)


m %>% broom::tidy()
m %>% broom::augment()
m %>% broom::glance()

m %>% broom::augment() %>% 
  ggplot(aes(weekPts,.fitted)) +
  geom_point() + 
  theme_minimal()

m$residuals %>% plot()

m %>% plot()
