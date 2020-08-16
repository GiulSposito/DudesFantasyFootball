library(tidyverse)
library(glue)

.week <- 4

# get fantasy team and week projection
pprojs  <- readRDS(glue("./data/week{.week}_players_projections.rds")) %>% 
  filter(avg_type=="weighted") %>% 
  mutate( fantasy.team = as.factor(fantasy.team) ) %>%
  select(id, nfl_id, fantasy.team, proj.floor=floor,
         proj.points=points, proj.ceiling=ceiling)

# calc historic data
points_stats <- readRDS("./data/players_points.rds") %>% 
  filter( week < .week ) %>%  
  select( id, nfl_id,week = fantasyPts.week.week, points = fantasyPts.week.pts ) %>% 
  mutate( 
    week = as.integer(week), 
    points = as.numeric(points)    
  ) %>%  
  group_by(id, nfl_id) %>% 
  summarise(
    pts.floor   = quantile(points, .05), 
    pts.median  = quantile(points, .5),
    pts.ceiling = quantile(points, .95)
  ) %>% 
  ungroup()

# get current players attributes
pattrib <- readRDS("./data/players_points.rds") %>% 
  filter( week == .week ) %>% 
  select(
    id, nfl_id, name, position, team, op.team = opponentTeamAbbr, injuryStatus,
    total.points = fantasyPts.season.pts
  ) %>% 
  mutate( 
    total.points = as.numeric(total.points),
    op.team = ifelse(op.team=="FALSE", "BYE", op.team)
  ) %>% 
  mutate( injuryCod = str_extract_all(injuryStatus, "[A-Z]")) %>% 
  mutate( injuryCod = map_chr(injuryCod, paste, sep="", collapse="")) %>%
  mutate( 
    name = case_when(
      !is.na(injuryStatus) ~ paste0(name, " (", injuryCod, ")"),
      op.team == "BYE" ~ paste0(name, " (bye)"),
      T ~ name
    )
  ) %>% 
  group_by(position) %>% 
  mutate( pos.rank = dense_rank(desc(total.points))) %>% 
  arrange(id)


players_history <- pattrib %>% 
  inner_join( pprojs, by = c("id", "nfl_id") ) %>% 
  inner_join( points_stats, by = c("id", "nfl_id") ) %>% 
  glimpse()

rm(pattrib, points_stats, pprojs); gc()

.pos <- "QB"
.n   <- 2.5*10
.title <- "Quaterbacks"

players_history %>% 
  filter( position == .pos ) %>% 
  arrange(pos.rank) %>%
  slice(1:.n) %>% 
  ggplot(aes(x=reorder(pos.rank, desc(pos.rank)), y=pts.median, color=fantasy.team,
             team=team, total.pts=total.points)) +
  geom_pointrange(aes(ymin=pts.floor, ymax=pts.ceiling), size=.8) +
  geom_text(aes(label=paste0("[", team, "] ", name), 
                y=pts.ceiling), hjust="left", vjust="middle", 
            nudge_y = 0.1, nudge_x = 0.1, size=3) +
  scale_colour_manual(values = c("grey",  "#e31a1c", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#fdbf6f", "#ff7f00", "purple", "pink","gold","cyan")) +
  ylab("historic peformance") + xlab("rank (total points)") +
  ggtitle(.title, glue("Top {.n} {.pos}s by Total Points - Historic Peformance (90% CI)")) +
  coord_flip() +
  theme_classic()


