# The possible types are: 'stats', 'twoWeekStats', 'fourWeekStats',
# 'projectedStats', 'restOfSeasonProjectedStats', 'researchStats',
# 'ranks', 'nflGames', 'advanced', 'rankAgainstPosition'.

json_players_stats_games <- ffa_players_stats_adv(config$authToken, config$leagueId, season, 1:16, "nflGames")
json_players_stats_ranks <- ffa_players_stats_adv(config$authToken, config$leagueId, season, 1:16, "ranks")
json_players_stats_adv <- ffa_players_stats_adv(config$authToken, config$leagueId, season, 1:16, "advanced")
json_players_stats_rankAgainst <- ffa_players_stats_adv(config$authToken, config$leagueId, season, 1:16, "rankAgainstPosition")
json_players_stats_research <- ffa_players_stats_adv(config$authToken, config$leagueId, season, 1:16, "researchStats")


adv_jsons <- list(
  nflGames = json_players_stats_games,
  ranks = json_players_stats_ranks,
  advanced = json_players_stats_adv,
  rankAgainstPosition = json_players_stats_rankAgainst,
  researchStats = json_players_stats_research
)

names(adv_jsons)

saveRDS(adv_jsons, "./data/advanced_players_stats_jsons.rds")

# ----
library(tidyverse)
stats <- readRDS("./data/advanced_players_stats_jsons.rds")

extractAdvStats <- function(playerList, statType){
  playerList %>% 
    map_df(function(.p, .n){
      st <- .p[[.n]]$week$`2020` %>% 
        map_df(function(.st){
          .st %>% 
            keep(~!is.null(.x)) %>%
            as_tibble() %>% 
            return()
        })
      st %>% 
        mutate(playerId=.p$playerId, week=names(.p[[.n]]$week$`2020`)[1:nrow(.)]) %>% 
        relocate(playerId) %>% 
        return()
    }, .n=statType)
}

game_stats <- extractAdvStats(stats$nflGames$content$games$`102020`$players, "nflGames") 
adv_stats <- extractAdvStats(stats$advanced$content$games$`102020`$players, "advanced")
ranks_stats <- extractAdvStats(stats$ranks$content$games$`102020`$players, "ranks")
res_stats <- extractAdvStats(stats$researchStats$content$games$`102020`$players, "researchStats")

advanced_stats <- game_stats %>% 
  inner_join(adv_stats, by = c("playerId", "week")) %>% 
  inner_join(ranks_stats, by = c("playerId", "week")) %>% 
  inner_join(res_stats, by = c("playerId", "week"))

library(httr)  
library(jsonlite)
resp <- GET("https://api.fantasy.nfl.com/v2/game/stats?appKey=internalemailuse") %>%
  content(as="text")

stid <- resp %>% 
  fromJSON()

stid <- stid$games$`102020`$stats %>% 
  map(discard, is.null) %>% 
  map_df(as_tibble)

saveRDS(stid, "./data/stats_id.rds")

stid <- stid %>% 
  bind_rows(
    tibble(
      id = "pts",
      abbr = "Pts",
      name = "Fantasy Points PPR",
      shortName = "PPR Pts", 
      scoringType = "points",
      isBonus = F, 
      groupName = "Points", 
      positionCategory = NA
    )
  ) %>% 
  distinct()

sim <- readRDS("./data/simulation_v5_week16_final.rds")

pstats <- sim$players_stats

players_stats <- pstats %>%  
  select(playerId, weekStats) %>% 
  mutate(weekStats = map(weekStats,function(.wst){
    week <- as.integer(names(.wst))
    stats <- map(.wst, function(.st){
      tibble(
        id    = as.character(names(.st)),
        value = as.numeric(unlist(.st))
      )
    })
    tibble(
      week  = week, 
      stats = stats
    ) %>% return()
  })) %>% 
  unnest(weekStats) %>% 
  unnest(stats)

# === databases ===

players_stats %>% 
  inner_join(stid, by="id") %>% 
  rename(statId = id) %>% 
  saveRDS("./data/ffadb/fantasy_stats.rds")

sim$players_stats %>% 
  select(playerId, name:byeWeek) %>% 
  saveRDS("./data/ffadb/players.rds")

game_stats %>% 
  mutate( across(c(playerId, week), as.integer) ) %>% 
  mutate( homeGame = !str_detect(opponentAbbr, "@") ) %>% 
  mutate( oppNflTeamAbbr  = str_remove_all(opponentAbbr, "@") ) %>% 
  relocate(playerId, week) %>% 
  saveRDS("./data/ffadb/games.rds")

adv_stats %>%
  mutate(across(c(-rushingYardsPerAttempt, -passingPercentage), as.integer)) %>% 
  mutate(across(c(rushingYardsPerAttempt, passingPercentage), as.numeric)) %>% 
  mutate(receptionPercentage=receptionPercentage/100) %>% 
  relocate(playerId, week) %>% 
  saveRDS("./data/ffadb/adv_stats.rds")

ranks_stats %>% 
  mutate_all(as.integer) %>% 
  relocate(playerId, week) %>% 
  saveRDS("./data/ffadb/ranks.rds")

res_stats %>% 
  mutate( across(c(playerId, week), as.integer)) %>% 
  mutate( across(percentStarted:percentOwnedChange, as.numeric)) %>% 
  relocate(playerId, week) %>% 
  saveRDS("./data/ffadb/research_stats.rds")
  


