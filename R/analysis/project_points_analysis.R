ptsproj %>% 
  filter(week==3) %>% 
  mutate(source_type=case_when(
    (data_src == "floor" | data_src == "ceiling") ~ "floor_ceiling",
    str_detect(data_src, "ERROR") ~ "site_error",
    T ~ "sites"
  )) %>% 
  ggplot(aes(x=pts.proj, fill=source_type)) +
  geom_density(alpha=.5) +
  facet_wrap(.~pos, scales = "free") +
  labs(title="Projection", subtitle = "Per Data Sources and Positions") +
  theme_minimal()


players_stats %>% 
  unnest(weekPts) %>% 
  select(playerId, position, week, weekPts) %>% 
  arrange(playerId) %>% 
  ggplot(aes(x=weekPts, fill=position)) +
  geom_density(alpha=.5) +
  facet_wrap(.~position) +
  labs(title="Points") +
  theme_minimal()

ptsproj %>% 
  ggplot(aes(x=pts.proj, fill=pos)) +
  geom_density(alpha=.5) +
  facet_wrap(.~pos) +
  labs(title="Projection") +
  theme_minimal()
