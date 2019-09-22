library(tidyverse)
library(plotly)

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
  ungroup() %>% 
  inner_join(proj, by="id") %>% 
  mutate( name = paste(first_name, last_name, sep = " ")) %>% 
  group_by(position) %>% 
  top_n(30, points.avg) %>% 
  ungroup()
  
g<- player.map %>% 
  #filter(position=="RB") %>% 
  top_n(60, points) %>% 
  ggplot(aes(x=points, y=points.avg, color=position, label=name)) +
  geom_point() +
  geom_errorbar(aes(ymin=points.avg-points.sd, ymax=points.avg+points.sd)) +
  geom_abline(aes(intercept=0, slope=1, group=2), linetype="dotted") +
  geom_errorbarh(aes(xmin=floor, xmax=ceiling)) +
  xlab("projeção") + ylab("historico") +
  xlim(0,30) + ylim(0,30) +
  theme_minimal()

ggplotly(g)

