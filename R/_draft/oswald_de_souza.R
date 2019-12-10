library(tidyverse)
library(e1071) # to use bincombinations

# case tests
w1 <- tibble(
  team = c("Bikers", "Riders", "Mules", "Kelloggs", "Rivers"),
  opp.team = c("Steelers", "Knights", "Giants", "Blues", "Robots")
)

w2 <- tibble(
  team = c("Riders", "Mules", "Kelloggs", "Rivers", "Bikers"),
  opp.team = c("Steelers", "Knights", "Giants", "Blues", "Robots")
)

w3 <- tibble(
  team = c("Mules", "Kelloggs", "Rivers", "Bikers", "Riders"),
  opp.team = c("Steelers", "Knights", "Giants", "Blues", "Robots")
)

# lista de semanas (rounds)
rounds <- list(w1,w2,w3)

# gera todos os resultados possives de um "round"
combRound <- function(.wround){
  # cria combinacoes binarias do numero de partidas
  bincombinations(nrow(.wround)) %>% 
    # separa cada uma das combinacoes possiveis
    split(1:nrow(.)) %>% 
    map(function(.x,.t){
      # adiciona os times as combinacoes
      mutate(.t, team.win=.x, opp.win=ifelse(.x==0,1,0))
    },.t=.wround) %>% 
    return()
}

# aplica as combinacoes a cada semana
rounds.results <- rounds %>% 
  map(combRound)

# vai gerar todas as combinacoes possiveis entre as semanas
results <- list(1:length(rounds.results[[1]])) %>% # numero de combinacoes por semana
  rep(length(rounds)) %>%                          # quantas semanas?
  expand.grid() %>%                                # combinacoes entre as semanas
  split(1:nrow(.)) %>%                             # para cada uma delas gera uma sequencia de jogos
  map(function(.x, .t){
    map2(.x, .t, function(.idx, .rnd){
      # retorna a combinacao de semanas/jogo possivel
      return(.rnd[[.idx]])
    })
  }, .t=rounds.results)


res <- results[[15]]

res %>% 


