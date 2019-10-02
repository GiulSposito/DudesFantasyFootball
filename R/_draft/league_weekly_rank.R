library(tidyverse)
library(lubridate)
library(glue)
library(ffanalytics)
library(ggimage)

# parametros de execucao
weeks <- 1:4

# check Fantasy API
source("./R/import/checkFantasyAPI.R")
if(!checkFantasyAPI(1)) stop("Unable to access Fantasy API!")

# import machups
source("./R/import/import_matchups.R")
matchups <- weeks %>% 
  map(
    importMatchups,
    .saveToFile = F
  )

# para cada time do roster tira informacoes de ponto, se ganhou e qual o rank
extractTeam <- . %>% 
  select(id, team=name, logoUrl, rank, pts, outcome) %>% 
  mutate_at(vars(id, rank), as.integer) %>% 
  mutate( win=(outcome=="win"), loss=(outcome!="win"), pts=as.numeric(pts) ) %>% 
  as_tibble()

# para cada jogo dentro da semana
# processa os times
extractGame <- function(.game) {
  
  .teams <- .game[[1]]$matchup 
  
  at <- extractTeam(.teams$awayTeam)
  ht <- extractTeam(.teams$homeTeam)
  
  at$pts.ctr <- ht$pts
  ht$pts.ctr <- at$pts
  
  bind_rows(at,ht) %>% 
    return()

}

# para cada conjunto de jogos dentre de uma semana
# processa o jogo
extractWeekGames <- function(.weekgames){
  .weekgames %>% 
    map_df(extractGame)
}

# para todas as semanas
wrank <- matchups %>% 
  map_df(
    extractWeekGames,
    .id="week"
  ) %>% 
  mutate( id=factor(id), team=factor(team) ) %>%
  arrange(week, rank)

# salva dados
saveRDS(wrank, "./data/weekly_results.rds")

wrank %>% 
  group_by(team) %>% 
  arrange(team,week) %>% 
  mutate( 
    wins = cumsum(win),
    losses = cumsum(loss),
    pts.pro = cumsum(pts),
    pts.ag  = cumsum(pts.ctr)
  ) %>% 
  ungroup()

wrank %>%
  ggplot(aes(x=week, y=pts, group=team)) +
  geom_line(aes(color=team), size=1) +
  geom_image(aes(image=logoUrl)) +
  theme_minimal()

wrank %>% 
  group_by(team) %>% 
  arrange(team, week) %>% 
  mutate( pts.cum = cumsum(pts) ) %>% 
  ungroup() %>% 
  ggplot(aes(x=week, y=pts.cum, group=team)) +
  geom_line(aes(color=team), size=1) +
  geom_image(aes(image=logoUrl), size=.03) +
  theme_minimal()

wrank %>% 
  group_by(team) %>% 
  arrange(team,week) %>% 
  mutate( 
    wins = cumsum(win),
    pts.pro = cumsum(pts),
    pts.ag  = cumsum(pts.ctr)
  ) %>% 
  ungroup() %>% 
  arrange(week, desc(wins), desc(pts.pro)) %>% 
  mutate( wrank = rep(1:10,4) ) %>% 
  ggplot(aes(x=week, y=reorder(wrank,-wrank), group=team)) +
  geom_line(aes(color=team), size=2) +
  geom_image(aes(image=logoUrl), size=.04) +
  geom_hline(yintercept = 6.5, linetype="dashed", size=1, color="darkgrey") +
  geom_text(x=4.4, y=6.65, label="playoff clinch", size=3, color="darkgrey") +
  ylab("rank") +
  ggtitle("Weekly Ranking") +
  theme_void()

