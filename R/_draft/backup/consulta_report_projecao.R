week <- 4

#### Consultas

# pega projecoes cria algumas colunas auxiliares
pprojs <- readRDS(glue("./data/week{week}_players_projections.rds")) %>% 
  mutate(injuryCod = str_extract_all(injuryGameStatus, "[A-Z]")) %>% 
  mutate(injuryCod = map_chr(injuryCod, paste, sep="", collapse="")) %>% 
  mutate( full_name = paste0(first_name, " ", last_name),
          fantasy.team = as.factor(fantasy.team) ) %>%
  mutate( 
    full_name = case_when(
      !is.na(injuryGameStatus) ~ paste0(full_name, " (", injuryCod, ")"),
      is.na(injuryGameStatus) ~ full_name
    )
  ) %>% 
  group_by( id ) %>% 
  mutate( r_pos = max(ceiling, points) ) %>% # posicao do label
  ungroup() %>% 
  filter( !(team %in% c("FA","FA*")) )  # havia lixo em alguns

ppoints <- readRDS("./data/players_points.rds") %>%
  mutate(id = as.integer(id)) %>% 
  filter( points!=0 ) %>% 
  rename( pts = points ) %>% 
  select( id, week, pts) %>% 
  group_by( id ) %>% 
  mutate( pts.mean = median(pts, na.rm = T),
          pts.sd   = sd(pts, na.rm=T),
          pts.max = max(pts, na.rm=T),
          pts.min = min(pts, na.rm = T)) %>% 
  ungroup() 

projections <- points %>% 
  right_join(projections, by = "id")

projections %>% 
  filter(position=="K") %>% View()

##### grafico

plotProjections <- function(.proj, .pos, .title, .subtitle, .slice=32) {
  
  # parameters
  .proj <- projections
  .pos  <- "K"
  .title <- "Kickers"
  .subtitle <- "Testando Grafico"
  .slice = 25
  
  require(magrittr)
  
  proj_data <- .proj %>% 
    select(id, position, pos_rank ) %>% 
    filter(position==.pos) %>% 
    distinct() %>% 
    top_n(-.slice, pos_rank) %>% 
    select(id)
  
  proj_data %>% 
    inner_join(.proj, by="id") -> proj_data
  
  nu_y <- max(proj_data$ceiling, na.rm = T) / 18
  
  proj_data %>% 
    ggplot(aes(team=team)) +
    geom_pointrange(aes(x=reorder(pos_rank,desc(pos_rank)),
                        y=points, 
                        ymin=floor, 
                        ymax=ceiling, 
                        color=fantasy.team), size=1) +

  geom_text(aes(x=reorder(pos_rank,desc(pos_rank)), 
                y=r_pos, 
                color=fantasy.team, 
                label=full_name), nudge_y = nu_y, nudge_x = 0.0, size=3) +
    scale_colour_manual(
      values = c("grey",  "#e31a1c", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#fdbf6f", "#ff7f00", "purple", "pink","gold","cyan"),
    ) +
    theme_classic() + xlab("Rank") + ylab("Points") +
    theme( panel.grid.major.x=element_line(colour="#EEEEEE") ) +
    scale_x_discrete(breaks = seq(1,32,4) ) +
    ggtitle(.title,.subtitle) +
    coord_flip() -> g
  
  g %>% 
    ggplotly() %>% 
    return()
  
}

plotProjections(projections, "QB", "Quaterbacks", "week 9", 30)

