library(tidyverse)
library(e1071)

w1 <- tibble(
  home.team = c("Bikers", "Riders", "Mules", "Kelloggs", "Rivers"),
  away.team = c("Steelers", "Knights", "Giants", "Blues", "Robots")
)

w2 <- tibble(
  home.team = c("Riders", "Mules", "Kelloggs", "Rivers", "Bikers"),
  away.team = c("Steelers", "Knights", "Giants", "Blues", "Robots")
)

w3 <- tibble(
  home.team = c("Mules", "Kelloggs", "Rivers", "Bikers", "Riders"),
  away.team = c("Steelers", "Knights", "Giants", "Blues", "Robots")
)

rounds <- list(w1,w2,w3)

combRound <- function(.wround){
  bincombinations(nrow(.wround)) %>% 
    split(1:nrow(.)) %>% 
    map(function(.x,.t){
      mutate(.t, team.win=.x)
    },.t=.wround) %>% 
    return()
}

rounds.results <- rounds %>% 
  map(combRound)


results <-list(1:length(rounds.results[[1]])) %>% 
  rep(length(rounds)) %>% 
  expand.grid() %>% 
  split(1:nrow(.)) %>% 
  map(function(.x, .t){
    map2(.x, .t, function(.idx, .rnd){
      return(.rnd[[.idx]])
    })
  }, .t=rounds.results)

