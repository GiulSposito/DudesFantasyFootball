#
library(tidyverse)
library(glue)
library(ggimage)

ranking <- 1:3 %>% 
  map(formatC, width = 2, flag = "0") %>% 
  map(~glue("./data/rank_week{.x}.rds")) %>% 
  map_df(readRDS)

sim <- readRDS("./data/simulation_v5_week3_posMNF.rds")

ranking %>% 
  mutate(record = paste0(wins, "-", losses)) %>% 
  # select(week, teamId, team=name, rank, record, imageUrl) %>% 
  inner_join(select(sim$teams, teamId, imageUrl), by="teamId") %>% 
  ggplot(aes(x=week, y=reorder(rank,-rank), group=team)) +
  geom_line(aes(color=team), size=2) +
  geom_image(aes(image=imageUrl), size=.04) +
  geom_text(aes(label=record), size=2.5, color="black", nudge_y = -.33) +
  geom_hline(yintercept = 8.5, linetype="dashed", size=1, color="darkgrey") +
  geom_text(x=1.4, y=8.65, label="playoff clinch", size=3, color="darkgrey") +
  geom_hline(yintercept = 12.5, linetype="dashed", size=1, color="darkgrey") +
  geom_text(x=1.4, y=12.65, label="bye", size=3, color="darkgrey") +
  ylab("rank") +
  theme_void()

ranking %>% 
  filter(week==3) %>% 
  mutate( 
    pct=wins/(wins+losses),
    img=glue("<img src='{imageUrl}' width='50px' height='50px'>")
  ) %>% 
  select(rank, rankChange, img, name, record, pct, streak, pts=season.pts, agains=season.ptsAgainst ) %>% 
  arrange(rank)




