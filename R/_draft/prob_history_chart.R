library(tidyverse)
library(ggimage)

sims <- readRDS("./data/simulations_history.rds")


sims %>% 
  filter(week==9) %>% 
  arrange(timestamp) %>%
  select(timestamp, away.name, home.name, away.win.prob, home.win.prob, away.logoUrl, home.logoUrl) %>% 
  mutate(
    away.team = str_extract(away.name,"\\b(\\w+)$"),
    home.team = str_extract(home.name,"\\b(\\w+)$"),
    game = paste0(away.team, " @ ", home.team),
    prob = 2*away.win.prob-1
  ) %>% 
  ggplot(aes(x=timestamp, y=prob)) +
  geom_line() +
  ylim(-1,1) +
  facet_wrap(~game, scales = "free")


sims %>% 
  filter(week==9) %>% 
  arrange(timestamp) %>%
  select(timestamp, away.name, home.name, away.win.prob, home.win.prob, away.logoUrl, home.logoUrl) %>% 
  mutate(
    away.team = str_extract(away.name,"\\b(\\w+)$"),
    home.team = str_extract(home.name,"\\b(\\w+)$"),
    game = paste0(away.team, " @ ", home.team)
  ) %>% 
  gather(key="prob",value="value", away.win.prob, home.win.prob) %>% 
  ggplot(aes(x=timestamp, y=value, fill=prob)) +
  geom_area() +
  geom_image(aes(x=min(timestamp), y=0, image=home.logoUrl), size=.15) +
  geom_image(aes(x=min(timestamp), y=1, image=away.logoUrl), size=.15) +
  geom_hline(yintercept = .5, linetype ="dashed") +
  ylim(-.1,1.1) +
  ylab("") +
  facet_wrap(~game, scales = "free") +
  theme_minimal() +
  theme( legend.position = "bottom", legend.title = element_blank(),
         axis.text.y = element_blank())

