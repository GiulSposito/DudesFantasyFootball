library(ffanalytics)

importNflIdMapping <- function(){
  
  # scrapping of NFL data from week 1 (all players)
  .scrap <- scrape_data( src="NFL",
                         pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                         season = 2018, week = 1 )
  
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
