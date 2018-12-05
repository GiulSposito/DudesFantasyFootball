source("./R/import/import_matchups.R")
source("./R/tidy/matchups.R")

1:13 %>% 
  map(importMatchups,
      saveToFile = F) -> rounds

countVictory <- . %>% 
  select(home.name, home.pts, away.pts, away.name) %>% 
  mutate(
    home.win = as.integer(home.pts>away.pts),
    away.win = as.integer(away.pts>home.pts)
  )

rounds %>% 
  map(extractTeams) %>% 
  map(countVictory) %>% 
  bind_rows(.id="week") %>% 
  select(home.name, away.name, home.win, away.win) -> games
  
tibble(
  team.A = c(games$home.name, games$away.name),
  team.B = c(games$away.name, games$home.name),
  victory.BA = c(games$home.win, games$away.win),
  victory.AB = c(games$away.win, games$home.win)
) -> victory.raw

victory.raw %>% 
  select(team.A, team.B, victory.AB) %>% 
  group_by(team.A, team.B) %>%
  summarise(victories=sum(victory.AB)) %>% 
  spread(team.A, victories) %>% 
  rename(Row.wins.Col = team.B) %>% View()

victory.raw %>% 
  group_by(team.A, team.B) %>%
  summarise(
    win = sum(victory.BA),
    los = sum(victory.AB)
  ) %>% View()



rounds %>% 
  map(extractTeams) %>% 
  map(countVictory) %>% 
  bind_rows(.id="week") -> gm.pts

tibble(
  week = as.integer(c(gm.pts$week,      gm.pts$week)),
  team = c(gm.pts$home.name, gm.pts$away.name),
  pts  = c(gm.pts$home.pts,  gm.pts$away.pts)
) %>% 
  group_by(week) %>% 
  mutate(rank = rank(pts)) %>% 
  ungroup() %>% 
  arrange(week, rank) -> team.points

library(plotly)

team.points %>% 
  ggplot( aes(x=week, y=rank, color=team) ) +
  geom_line(size=2) +
  geom_point(aes(label=pts), size=4, label.padding = unit(2, "points"), show.legend = F) +
  theme_minimal() +
  theme( legend.position = "bottom" ) -> g

saveRDS(g, "./data/ggplot_points_rank.rds")


team.points %>% 
  ggplot( aes(x=week, y=pts, color=team) ) +
  geom_line(size=2) +
  geom_point(aes(label=pts), size=4, label.padding = unit(2, "points"), show.legend = F) +
  theme_minimal() +
  theme( legend.position = "bottom" ) -> g


saveRDS(g, "./data/ggplot_points_record.rds")


