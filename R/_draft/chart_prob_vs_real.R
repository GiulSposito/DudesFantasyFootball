library(tidyverse)
library(ggridges)
library(glue)
library(ggimage)

sim <- readRDS("./data/simulation_v5_week6_final.rds")

inner_join(
  select(sim$players_sim, teamId, teamName, playerId, rosterSlotId, simulation.org),
  select(sim$players_stats, playerId, name, position, weekPts),
  by="playerId"
) %>% 
  unnest(weekPts) %>% 
  filter(week==6, rosterSlotId < 20) %>% 
  unnest(simulation.org) %>% 
  #filter(teamId==10) %>% 
  mutate(name=glue("{name} ({position})")) %>% 
  mutate(position2=factor(position, levels = c("QB","WR","RB", "TE", "K", "DEF"),ordered = T)) %>% 
  # mutate(name=fct_reorder(name, -as.integer(position2))) %>% 
  mutate(name=fct_reorder(name, simulation.org)) %>% 
  ggplot(aes(x=simulation.org, y=name, fill=position)) +
  #geom_density_ridges() +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.1,0.5,0.9), scale=1, alpha=.6) +
  geom_point(aes(x=weekPts, y=name), color="black", fill="red", shape=24) +
  theme_ridges() +
  theme( legend.position = "none" ) +
  scale_y_discrete(expand=c(0.1,0)) +
  scale_x_continuous(expand = c(0.01,0)) +
  labs(x="points",y="") +
  facet_wrap(teamName~., scales = "free")


week_points <- 1:6 %>% 
  map_df(function(.w){
    readRDS(glue("./data/simulation_v5_week{.w}_final.rds")) %>% 
      pluck("teams") %>% 
      select(teamId, name, imageUrl, week.stats) %>% 
      unnest(week.stats) %>% 
      return()
  })

rank_pts <- week_points %>% 
  group_by(teamId) %>% 
  arrange(teamId, week) %>% 
  mutate(cumpts = cumsum(pts)) %>% 
  ungroup() %>% 
  arrange(week, desc(cumpts)) %>% 
  mutate(rank=rep(1:14,6)) %>% 
  rename(team=name)

TEAMS_COLOR_SCALE <- c("#B3995D","black","#03202F","#0B2265","#fb9a99","#A5ACAF","#0085CA",
                       "#ff7f00","#203731","#6a3d9a","#A71930","#A71930","#FFB612","#004953")

rank_pts %>% 
  # mutate(record = paste0(wins, "-", losses)) %>% 
  # select(week, teamId, team=name, rank, record, imageUrl) %>% 
  # # inner_join(select(sim$teams, teamId, imageUrl), by="teamId") %>% 
  ggplot(aes(x=week, y=reorder(rank,-rank), group=team)) +
  geom_line(aes(color=team), size=2) +
  geom_image(aes(image=imageUrl), size=.04) +
  geom_text(aes(label=cumpts), size=2.5, color="black", nudge_y = -.40) +
  # geom_hline(yintercept = 8.5, linetype="dashed", size=1, color="darkgrey") +
  # geom_text(x=1.4, y=8.65, label="playoff clinch", size=3, color="darkgrey") +
  # geom_hline(yintercept = 12.5, linetype="dashed", size=1, color="darkgrey") +
  # geom_text(x=1.4, y=12.65, label="bye", size=3, color="darkgrey") +
  scale_colour_manual(values = TEAMS_COLOR_SCALE) +
  ylab("rank") +
  theme_void() +
  theme(legend.position = "bottom")
