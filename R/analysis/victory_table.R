source("./R/import/import_matchups.R")
source("./R/tidy/matchups.R")

countVictory <- . %>% 
  select(home.name, home.pts, away.pts, away.name) %>% 
  mutate(
    home.win = as.integer(home.pts>away.pts),
    away.win = as.integer(away.pts>home.pts)
  )

1:13 %>% 
  map(importMatchups,
      saveToFile = F) -> rounds



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
  pts  = c(gm.pts$home.pts,  gm.pts$away.pts),
  win = c(gm.pts$home.win, gm.pts$away.win),
  loss = c(gm.pts$away.win, gm.pts$home.win)
) %>% 
  group_by(week) %>% 
  mutate(pts.rank = rank(desc(pts))) %>% 
  ungroup() %>% 
  arrange(week, pts.rank) %>% 
  group_by(team) %>% 
  mutate(
    points = cumsum(pts),
    wins   = cumsum(win),
    losses = cumsum(loss)
  ) %>% 
  ungroup() %>% 
  group_by(week) %>% 
  mutate(win.rank = rank(desc((wins-losses)*10000+points))) %>% 
  ungroup() %>% 
  arrange(week, win.rank) -> team.points

library(plotly)

team.points %>% 
  ggplot( aes(x=week, y=win.rank, color=team) ) +
  geom_line(size=2) +
  geom_label(aes(label=paste(wins, losses, sep="-")), size=4, label.padding = unit(2, "points"), show.legend = F) +
  theme_void() +
  theme( legend.position = "bottom" ) +
  theme(panel.grid.major.y = element_blank()) +
  scale_y_reverse() +
  geom_text(data=filter(team.points, week==1), aes(label=team, x=-1.5, color=team) , hjust = -.05, size = 3.5, fontface=2, show.legend=F) +
  geom_text(data=filter(team.points, week==13), aes(label=team, x=15.5, color=team) , hjust = 1.05, size = 3.5, fontface=2, show.legend=F)



geom_text(data = df.world_ports %>% filter(year == "2014", rank <= param.rank_n), aes(label = port_label, x = '2014') , hjust = -.05, color = "#888888", size = 4) +
  geom_text(data = df.world_ports %>% filter(year == "2004", rank <= param.rank_n), aes(label = port_label, x = '2004') , hjust = 1.05, color = "#888888", size = 4) +
  



saveRDS(g, "./data/ggplot_points_rank.rds")


team.points %>% 
  ggplot( aes(x=week, y=pts, color=team) ) +
  geom_line(size=2) +
  geom_point(aes(label=pts), size=4, label.padding = unit(2, "points"), show.legend = F) +
  theme_minimal() +
  theme( legend.position = "bottom" ) -> g


saveRDS(g, "./data/ggplot_points_record.rds")


