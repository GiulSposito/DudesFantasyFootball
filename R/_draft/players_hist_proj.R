library(tidyverse)

proj <- readRDS("./data/week3_players_projections.rds")
pts  <- readRDS("./data/players_points.rds")

player.map <- pts %>% 
  select(id, injuryStatus, week = fantasyPts.week.week, points = fantasyPts.week.pts) %>% 
  mutate( week = as.integer(week), 
          points = as.numeric(points)) %>%
  group_by(id, injuryStatus) %>% 
  summarise( 
    points.avg = mean(points),
    points.sd  = sd(points), 
    points.max = max(points), 
    points.min = min(points)
  ) %>% 
  inner_join(proj, by="id") %>% 
  ungroup()

  
g<- player.map %>% 
  #filter(position=="RB") %>% 
  top_n(60, points) %>% 
  ggplot(aes(x=points, y=points.avg, color=position)) +
  geom_point() +
  geom_errorbar(aes(ymin=points.avg-points.sd, ymax=points.avg+points.sd)) +
  geom_errorbarh(aes(xmin=floor, xmax=ceiling)) +
  xlab("projeção") + ylab("historico") +
  theme_minimal()

library(plotly)
ggplotly(g)
