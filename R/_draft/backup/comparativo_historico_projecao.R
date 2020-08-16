library(tidyverse)
library(plotly)

# pontuacao "cru" dos sites
# projection <- readRDS("./data/points_projection.rds")
# projection %>% glimpse()

# points table
prj <- 1:3 %>% 
  map_df(~readRDS(glue("./data/week{.x}_players_projections.rds")), .id = "week") %>% 
  mutate(week=as.integer(week)) %>% 
  select(1:18, nfl_id) %>% 
  rename(proj.pts=points)


# pontuacao fantasy
pts <- readRDS("./data/players_points.rds")
glimpse(pts)

pdata <- pts %>%
  select(id, nfl_id, week=fantasyPts.week.week, points=fantasyPts.week.pts, injuryStatus) %>%
  mutate(
    week   = as.integer(week),
    points = as.numeric(points)
  ) %>% 
  inner_join(prj, by = c("id", "nfl_id", "week")) %>% 
  mutate(injuryCod = str_extract_all(injuryStatus, "[A-Z]")) %>% 
  mutate(injuryCod = map_chr(injuryCod, paste, sep="", collapse="")) %>% 
  mutate( full_name = paste0(first_name, " ", last_name),
          fantasy.team = as.factor(fantasy.team) ) %>%
  mutate( 
    full_name = case_when(
      !is.na(injuryStatus) ~ paste0(full_name, " (", injuryCod, ")"),
      is.na(injuryStatus) ~ full_name
    )
  )



g <- pdata %>% 
  filter(position=="QB") %>% 
  ggplot(aes(y=points, x=proj.pts, color=position, name=full_name)) +
  geom_point() +
  geom_errorbarh(aes(xmin=floor, xmax=ceiling)) +
  geom_abline(aes(intercept=0, slope = 1), linetype="dashed") +
  theme_minimal()

ggplotly(g)

 
