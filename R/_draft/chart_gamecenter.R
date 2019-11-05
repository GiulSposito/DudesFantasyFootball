library(tidyverse)
library(glue)

.week <- 9
sim <- readRDS("./data/simulations_history.rds") %>% 
  filter(week==.week) %>%
  filter(timestamp==max(timestamp, na.rm=T))

lastUpdate <- unique(sim$timestamp) 
lastPrefix <- unique(sim$prefix)

sim %>%
  unnest(home.points, away.points, .sep = ".") %>%
  select(week, timestamp, prefix, 
         contains("logoUrl"),
         contains("name"),
         contains("record"),
         contains("win.prob"),
         contains("pts"),
         contains("Median"),
         contains("minutesRemaining")) %>% 
  mutate( game.id = glue("({away.minutesRemaining} min) {away.name} @ {home.name} ({home.minutesRemaining} min)") ) %>%
  ggplot(aes(x=0)) +
  geom_col(aes(y=home.points.Median), fill="lightgray", width = .7) +
  geom_col(aes(y=home.pts), fill="darkred", width = .5) +
  geom_col(aes(y=-away.points.Median), fill="lightgray", width = .7) +
  geom_col(aes(y=-away.pts), fill="darkblue", width = .5) +
  geom_text(aes(y=home.points.Median, label=round(home.points.Median,2)), size=4,
            hjust = "left", vjust="middle", color="black", nudge_y = 2, nudge_x = .05) +
  geom_text(aes(y=home.pts, label=round(home.pts,2)), size=4,
            hjust = "right", vjust="middle", color="white", nudge_y = -2, nudge_x = .05) +
  geom_text(aes(y=-away.points.Median, label=round(away.points.Median,2)), size=4,
            hjust = "right", vjust="middle", color="black", nudge_y = -2, nudge_x = .05) +
  geom_text(aes(y=-away.pts, label=round(away.pts,2)), size=4,
            hjust = "left", vjust="middle", color="white", nudge_y = 2, nudge_x = .05) +
  coord_flip() +
  facet_wrap(~game.id, ncol = 1) +
  labs(x="", title=glue("Last Update: {lastUpdate} - {lastPrefix}"),
       subtitle = "") +
  xlim(-.5,.5) +
  theme_void() +
  theme( strip.text = element_text(size=12, face = "bold"), 
         plot.margin = unit(c(15,15,15,15),"pt"),
         plot.title = element_text(hjust = .5, size=10),
         plot.subtitle = element_text(hjust = .5) )
