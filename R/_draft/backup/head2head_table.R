# libs
library(tidyverse)
library(glue)
# library(ggimage)
# library(knitr)
# library(kableExtra)

# parameter
curr.week <- 12

# game results
matchups <- readRDS("./data/post_matchups_results.rds")

# para cada time do roster tira informacoes de ponto, se ganhou e qual o rank
extractTeam <- . %>% 
  select(id, team=name, logoUrl, pts, outcome) %>% 
  mutate(
    id = as.integer(id),
    win=(outcome=="win"),
    loss=(outcome!="win"),
    pts=as.numeric(pts)
  ) %>% 
  as_tibble()

# adiciona informacoes do oponente para fazer o ranking H2H
addOpponent <- function(.t, .o){ 
  .t %>% 
    mutate(
      pts.ctr = .o$pts,
      op.id   = .o$id,
      op.team = .o$team,
      op.out  = .o$outcome
    ) %>% 
    return()
}

# para cada jogo dentro da semana
# processa os times
extractGame <- function(.game) {
  
  .teams <- .game[[1]]$matchup 
  
  at <- extractTeam(.teams$awayTeam)
  ht <- extractTeam(.teams$homeTeam)
  
  at <- addOpponent(at, ht)
  ht <- addOpponent(ht, at)
  
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
  mutate(week=as.integer(week))

# para a geracao no confronto direto
# por semana computa o h2h balance time a time
h2h.weekly <-  wrank %>% 
  select(week, id, team, outcome, op.out, op.team, op.id) %>% 
  mutate( h2h.balance = (outcome=="win") - (op.out=="win") )

# entao monta uma tabela acumulando semana a semana
# exemplo, na semana 3, na verdade tem a soma do h2h da semana 1, 2 e 3
h2h.table <-  1:curr.week %>% # quantas semanas?
  map(~seq(1,.x)) %>% # gera uma lista de semanas (1, 1:2, 1:3, 1:4, ...)
  map_df(function(.weeks, .h2hw){
    .h2hw %>% 
      filter(week %in% .weeks) %>% # vai acumulando 
      group_by(id, team, op.id, op.team) %>% 
      summarise( balance = sum(h2h.balance)) %>% 
      ungroup() %>% 
      mutate(week = max(.weeks)) %>% 
      return()
  }, 
  .h2hw = h2h.weekly)

# computa time time, semana a semana o 
# numero de vitorias, terrotas, pontos pro e contra
wrank <- wrank %>% 
  group_by(team) %>% 
  arrange(team,week) %>% 
  mutate( 
    wins = cumsum(win),
    losses = cumsum(loss),
    pts.pro = cumsum(pts),
    pts.ag  = cumsum(pts.ctr)
  ) %>% 
  ungroup() %>% 
  filter(week <= curr.week)

# agrupa, semana a semana os grupos de time com mesmo
# numero de vitorias e derrotas
finalrank <- wrank %>% 
  mutate( .week = week ) %>% # precisa aninhar a semana para passar para a h2h.table dentro do grupo
  group_by(week, wins, losses) %>% 
  nest() %>% 
  mutate(
    data = map(data, function(.x, .h2ht){
      
      # descobre a semana do grupo
      sel.week <- unique(.x$.week)
      
      # filtra qual eh a h2h table que importa
      wh2h <- filter(.h2ht, week==sel.week)
      
      # gera as comubinações entre todos os oponentes do grupo
      expand.grid(id=.x$id, op.id=.x$id) %>% 
        # junta com o balanco h2h
        inner_join(wh2h, by = c("id", "op.id")) %>% 
        group_by(id, team) %>% 
        # calcula o balanco entre os times do grupo
        summarise(h2h.balance=sum(balance)) %>% 
        ungroup() %>% 
        # recompoem com dados do resultado da semana
        right_join(.x, by = c("id", "team")) %>% 
        # zera o balanco se houver NA
        mutate( h2h.balance = ifelse(is.na(h2h.balance),0,h2h.balance) ) %>% 
        select(-.week) %>% 
        return()
    },
    .h2ht=h2h.table)
  ) %>% 
  unnest(data) %>% 
  arrange(week, desc(wins), desc(h2h.balance), desc(pts.pro), pts.ag) %>% 
  mutate( wrank = rep(1:10,curr.week) ) %>% 
  select(-op.id, -op.team, -op.out)

h2h.table %>% 
  filter(week==12) %>% 
  select(team, op.team, balance) %>%
  mutate( team    = str_extract(team,    "\\b(\\w+)$"),
          op.team = str_extract(op.team, "\\b(\\w+)$") ) %>% 
  mutate( team = fct_reorder(team, balance, mean)) %>% 
  ggplot(aes(x=op.team, y=team)) +
  geom_tile(aes(fill=balance)) +
  geom_text(aes(label=balance)) +
  scale_fill_gradient2(low = "pink", mid="white", high="lightgreen") +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle=45), 
         legend.position = "none") +
  labs(title="Head to Head Table",
       subtitle="# of 'team' victores over 'opponent' - Week 12",
       y="team", x="opponent")

