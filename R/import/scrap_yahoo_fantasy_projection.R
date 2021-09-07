library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(janitor)
library(ffanalytics)

scrapYahooPosition <- function(.pos, .size, .week, .cookies){

  scrp_data <- seq(0,.size,25) %>%
    map_df(function(.i, .pos, .week, .cookies){

      # weekly
      if (.week!=0) {
        # weekly projection
        yahooUrl <- glue("https://football.fantasysports.yahoo.com/f1/992851/players?status=ALL&pos={.pos}&cut_type=9&stat1=S_PW_{.week}&myteam=0&sort=AR&sdir=1&count={.i}")
        #yahooUrl <- glue("https://football.fantasysports.yahoo.com/f1/992851/players?status=ALL&pos={.pos}&cut_type=9&stat1=S_PW_{.week}&myteam=0&sort=AR&sdir=1&count={.i}")
      } else {
        # season projection
        yahooUrl <- glue("https://football.fantasysports.yahoo.com/f1/992851/players?status=A&pos={.pos}&cut_type=9&stat1=S_PS_2021&myteam=0&sort=AR&sdir=1&count={.i}")
      }
      
      print(paste0(yahooUrl,"\n"))
      
      resp <- GET(
        url = yahooUrl,
        add_headers(cookie=.cookies)
      )

      tables <- resp %>%
        content(as="text") %>%
        read_html() %>%
        html_table()

      if(.pos!="DEF"){
        ids <- resp %>%
          content(as="text") %>%
          read_html() %>%
          html_nodes(xpath="//*[@class='Nowrap name F-link']") %>%
          html_attr(name = "href") %>%
          str_extract("[0-9]+")
      } else {
        ids <- resp %>%
          content(as="text") %>%
          read_html() %>%
          html_nodes(xpath="//*[@class='Nowrap name F-link']") %>%
          html_attr(name = "href") %>%
          str_extract("\\b(\\w+)/$") %>%
          str_remove_all("/")
      }

      stats <- tables[[1]]

      new_names <- stats[1,] %>%
        unlist() %>%
        paste(names(stats),., sep=" ") %>%
        make_clean_names()

      stats[-1,] %>%
        set_names(new_names) %>%
        as_tibble() %>%
        mutate(src_id=ids) %>% 
        return()


    }, .pos=.pos, .week=.week, .cookies=.cookies) %>%
    distinct()

  names(scrp_data)[2] <- "playerData"

  scrp_data %>%
    mutate( map_df(playerData, ~{
      .x %>%
        str_split("\n") %>%
        map_df(~{
          y <- .x[2] %>%
            str_split(" - ",simplify = T) %>%
            str_trim()
          tibble(
            name = y[1],
            position  = y[2],
            injuryStatus=str_trim(.x[5])
          )
        })
    })) %>%
    mutate(
      team = str_to_upper(str_extract(name, "\\b(\\w+)$")),
      player = str_remove_all(name," \\b(\\w+)$")
    ) %>%
    select(-x, -x_2, -playerData, -roster_status, -name) %>%
    select(src_id, player, position, team, everything()) %>%
    return()
}

scrapYahooProjection <- function(.week, .yahooCookies){

  scrp_df <- tibble(
      pos = c("O","K","DEF"),
      size= c(1025, 50, 25)
    ) %>%
    split(1:nrow(.)) %>%
    map_df(function(.position, .week, .cookies){
      scrapYahooPosition(.position$pos, .position$size, .week=.week, .cookies = .cookies)
    }, .week=.week, .cookies=.yahooCookies) %>%
    select(-na, -injuryStatus) %>% 
    select(-one_of("forecast"))
    
  
  ffa_columns = tibble(
    colName = names(projection_sources[["Yahoo"]]$stat_cols),
    shortName = projection_sources[["Yahoo"]]$stat_cols,
    janName   = make_clean_names(shortName)
  ) 

  
  table_cols <- ncol(scrp_df)
    
  scrp <- scrp_df %>%
    mutate(trends_percent_rostered=as.numeric(str_remove_all(trends_percent_rostered,"%"))/100) %>%
    mutate(across(all_of(5:table_cols), as.numeric)) %>% 
    pivot_longer(cols = -c(src_id, player, position, team), names_to="janName", values_to="value") %>%
    inner_join(ffa_columns, by="janName") %>%
    pivot_wider(id_cols=c(src_id, player, position, team), names_from=colName, values_from=value) %>%
    mutate(data_src="Yahoo") %>%
    # modifica chave do src_id quando o time é de defesa (há dois "bay")
    mutate( src_id = case_when(
      position=="DEF" ~ str_c(src_id, team, sep="-"),
      T ~ src_id
    )) %>% 
    select(data_src, everything())
  
  ## IDS do FFA
  # carregando tabelas de "de para" de IDs de Jogadores
  load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!

  # scrp %>% 
  #   inner_join(player_ids, by=c("src_id"="stats_id"))
  
  # mapeamento de src_ids (como yahoo_id)
  yahoo_id_map <- readRDS("./data/yahoo_id_map.rds") %>% 
    inner_join(player_ids, by="id")
  
  # salva yahoo players "scrapeados" sem mapeamento para o ffa_id
  scrp %>% 
    anti_join(yahoo_id_map, by=c("src_id"="yahoo_id")) %>% 
    saveRDS("./data/yahoo_players_not_imported.rds")
  
  # bind scrap do yahoo com o player id (ffa)
  scrp %>% 
    inner_join(yahoo_id_map, by=c("src_id"="yahoo_id")) %>% 
    mutate(position = if_else(position=="DEF","DST",position)) %>% 
    select(data_src, id, src_id, player, position, team, everything()) %>% 
    filter(!is.na(id)) %>% 
    return()
  
}

