library(tidyverse)
library(glue)

.week <- 5

players <- readRDS("./data/players.rds")
stats   <- readRDS("./data/players_points.rds")
proj    <- readRDS(glue("./data/week{.week}_players_projections.rds"))

bye_week <- players %>% 
  select(nfl_id=playerId, byeWeek)

player.summary <- stats %>% 
  select(nfl_id, week, points) %>% 
  inner_join(bye_week, by = "nfl_id") %>% 
  filter( week!=byeWeek, week <= .week ) %>% 
  arrange(nfl_id) %>% 
  group_by(nfl_id) %>% 
  summarise(
    pts.mean = mean(points),
    pts.sd   = sd(points),
    pts.10   = quantile(points, .1),
    pts.median = median(points),
    pts.90   = quantile(points, .9),
    played.weeks = n()
  ) %>% 
  ungroup() %>% 
  arrange(desc(pts.10))

# monta tabela de infor de players

p1 <- players %>% 
  select(nfl_id = playerId, smallImageUrl, byeWeek)

p2 <- stats %>% 
  filter(week==(.week+1)) %>% 
  select(nfl_id, name, position, team, opponentTeamAbbr, injuryStatus, leagueStatus)

p3 <- proj %>%
  select(nfl_id, fantasy.team, injuryGameStatus, points, floor, ceiling) %>% 
  glimpse()

players.info <- p2 %>% 
  inner_join(p1, by = "nfl_id") %>% 
  inner_join(player.summary, by = "nfl_id") %>% 
  inner_join(p3, by = "nfl_id") %>% 
  filter(fantasy.team %in% c("*FreeAgent","Bikers"), 
         is.na(injuryStatus)) 


st01 <- tibble(
  pos=c("QB","RB","WR","TE","K","DEF"),
  qtd=c(1,2,2,1,1,1)
) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      filter(position==.x$pos) %>%
      top_n(.x$qtd, pts.median)
  }, .players=filter(players.info, byeWeek != .week, opponentTeamAbbr!="FALSE")
  )

starters <- players.info %>% 
  filter(byeWeek != .week, opponentTeamAbbr!="FALSE") %>% 
  filter(position %in% c("WR","RB")) %>% 
  anti_join(st01) %>% 
  top_n(1, pts.median) %>% 
  bind_rows(st01,.)

notstarters <- players.info %>% 
  anti_join(starters)

bench <- tibble(
    pos=c("QB","RB","WR","TE"),
    qtd=c(1,2,2,1)
  ) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.x, .players){
    .players %>% 
      filter(position==.x$pos) %>%
      top_n(.x$qtd, pts.median)
  }, .players=filter(notstarters, byeWeek != .week, opponentTeamAbbr!="FALSE"))


starters <- starters %>% 
  select(name, position, team, points, fantasy.team, opponentTeamAbbr, 
         injuryStatus, injuryGameStatus, everything())

bench <- bench %>% 
  select(name, position, team, points, fantasy.team, opponentTeamAbbr, 
         injuryStatus, injuryGameStatus, everything())

# release
release <- players.info %>% 
  filter(fantasy.team=="Bikers") %>% 
  anti_join(bind_rows(starters, bench))%>% 
  select(name, position, team, points, fantasy.team, opponentTeamAbbr, 
         injuryStatus, injuryGameStatus, everything())

bind_rows(
  starters,
  bench,
  release
) %>% 
  mutate( rnk = 1:nrow(.) ) %>% 
  ggplot(aes(x=reorder(rnk,-rnk), y=pts.median, color=fantasy.team)) +
  geom_point() +
  geom_errorbar(aes(ymin=pts.10, ymax=pts.90)) +
  coord_flip() +
  theme_minimal()

bind_rows(
  starters,
  bench,
  release
) %>% 
  select(position, name, fantasy.team, pts.10, pts.median, points, pts.90, pts.sd, injuryStatus) %>% 
  View()
