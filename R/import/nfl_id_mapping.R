library(ffanalytics)

extract.nfl.id <- function(.scrap){
  # isolate ID of NFL and ID of ffanalytics
  .scrap %>%
    map(function(dft){
      dft %>% 
        filter(data_src=="NFL") %>% 
        select(id, src_id, player, team, pos) %>% 
        return()
    }) %>% 
    bind_rows() %>%
    distinct() %>% 
    mutate_at(vars(id,src_id), as.integer) %>% 
    as.tibble() %>% 
    return()
}

1:7 %>% 
  map(function(.week){
    scrape_data( src="NFL",
                 pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                 season = 2018, week = .week )
  }) %>% 
  map(extract.nfl.id) %>% 
  bind_rows() %>% 
  distinct() -> player.ids

saveRDS(player.ids, "./data/nfl_players_id.rds")

