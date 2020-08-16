library(tidyverse)

# carregando pontos e projeções
points <- readRDS("./data/players_points.rds")
projections <- readRDS("./data/players_projections.rds") %>% 
  rename(pts.proj = points)

# montando dataset de comparação pontos e projecao
points %>% 
  inner_join(projections, by = c("src_id", "season", "week")) %>%
  select(id, player, position, season, week, points, data_src, pts.proj) %>% 
  mutate( data_src = as.factor(data_src),
          position = as.factor(position),
          error = (pts.proj-points) ) -> player.data

# Plotando a projeção contra a pontuação
player.data %>% 
  ggplot(aes(x=points, y=pts.proj)) +
  geom_point(aes(color=position)) +
  geom_abline(intercept = 0, slope=1, linetype=2) +
  facet_grid( position~data_src ) +
  theme_bw() +
  theme( legend.position = "none" )

# Erro é constante alo longo dos pontos projetados?
player.data %>% 
  # filter(data_src=="CBS",position=="WR", !is.na(error)) %>% 
  ggplot(aes(x=pts.proj, y=error)) +
  geom_point(aes(color=position)) +
  facet_grid( position~data_src ) +
  theme_bw() +
  theme( legend.position = "none" )

# distribuição de erro por posicao e site
player.data %>% 
  filter( !(points==0 & pts.proj==0) ) %>% 
  ggplot() +
  geom_histogram(aes(error, fill=position)) +
  facet_grid( position~data_src ) +
  theme_bw() +
  theme( legend.position = "none" )

# pegando um dos casos Wide Receiver na CBS
player.data %>% 
  filter(data_src=="CBS",position=="WR", !is.na(error)) %>% 
  ggplot() +
  geom_histogram(aes(error, fill=position)) +
  theme_bw() +
  theme( legend.position = "none" )

# comparando com uma curva amostrada
player.data %>% 
  filter(data_src=="CBS",position=="WR", !is.na(error)) %>% 
  pull(error) %>% 
  sample(size=200, replace = T) %>% 
  hist(col="red", breaks = 50)

# quantos pontos ha por site e posicao ?
table(player.data$position, player.data$data_src)
