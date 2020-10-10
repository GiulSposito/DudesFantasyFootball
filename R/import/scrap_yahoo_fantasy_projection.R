library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(janitor)
library(ffanalytics)

.scrapYahooPosition <- function(.pos, .size, .week, .cookies){

  scrp_data <- seq(0,.size,25) %>%
    map_df(function(.i, .pos, .week, .cookies){

      yahooUrl <- glue("https://football.fantasysports.yahoo.com/f1/1196449/players?status=ALL&pos={.pos}&cut_type=9&stat1=S_PW_{.week}&myteam=0&sort=AR&sdir=1&count={.i}")
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
        mutate(src_id=ids)


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

# api_cookies <- "F=d=RzzNpk09vBjdRptI2CXYtkm3OHpjMLBMX9Vuuad7Lw--; AO=u=1; OTH=v=1&d=eyJraWQiOiIwMTY0MGY5MDNhMjRlMWMxZjA5N2ViZGEyZDA5YjE5NmM5ZGUzZWQ5IiwiYWxnIjoiUlMyNTYifQ.eyJjdSI6eyJndWlkIjoiTTI1TjRKNVI0UkRNNzZKSFYyNDNRTEZIVFEiLCJwZXJzaXN0ZW50Ijp0cnVlLCJzaWQiOiJoZkhYcTI4RDk1eUQifX0.fsgbeTmDW2sA3CcFNkx-0To9S-cTgLM1-6YcF4ersPS_rhwrlMFCAI-vNQ8Nr6Wdz5NXfFr7FwkSj8LwVXtRuBjHe_HrAM-QzBo_nqBl02nHJQbY6ho31FZZg5Rj9I167fniNYeSNashp4h1wNF69cPXDdk15z7trRNVC8skYRA; T=af=JnRzPTE2MDE4MTYwMDAmcHM9NEVMR1hyYWhzaWtPYmxSVnZtRkRody0t&d=bnMBeWFob28BZwFNMjVONEo1UjRSRE03NkpIVjI0M1FMRkhUUQFhYwFBSEYwY2k3YQFzYzABAWNzAQFhbAFnaXVsc3Bvc2l0bwFzYwFkZXNrdG9wX3dlYgFmcwFRM2tvRU9aZmVjVzcBenoBN1djZWZCQTdFAWEBUUFFAWxhdAE3V2NlZkI-&sk=DAAXutkClRyzVF&ks=EAAjFyCOS5mDItF2Fap0nlFcw--~G&kt=EAAwjjdwgUnh.tN9whkdEJtPQ--~I&ku=FAAwnypDueC2maGD_VnnPP_3o2ctLSMaRwDC1tMOARs4tbw9.j8RoIGdyxeJSifEBey.wGjUw8PIF6CHw9MFAoBVQE4FOtviVP_fnWO3FCwIqGyNtnx3SP1KZVALGWXnpXA8kRn215w7Z1Blune1c0r5ePIlcl69GPrrVsrVfQVVSg-~D; PH=fn=o9cU0j23r94R0c.6j00kRNo-&i=us; Y=v=1&n=4ij1tg6ststt3&l=68kbifei8je/o&p=m2bvvbr00000000&ig=3vvjv&r=66&intl=br; SPT=d=N2uT_uJkWfoaPqTtJ24Q3bCwlR.5VObwxz2Mbk6H7uF3a3vFs1ouWfN92Hf1UBGv8Jnv_fKjF1cy99tZv8ON.HB16JRRinJK15o7TnaclGpGI8YtFfaPlV5zX8gr&v=1; A1=d=AQABBFOrEF8CEMP-aWV8KHc-RRsdjqYMpVQFEgEAAgILe19fYB4Ab2UB_SMAAAcIU6sQX6YMpVQID3ze1SwEMWi3GJP7mrlosgkBBwoBSw&S=AQAAAtCuUt7EfsPszBLTpkM3Y2I; A3=d=AQABBFOrEF8CEMP-aWV8KHc-RRsdjqYMpVQFEgEAAgILe19fYB4Ab2UB_SMAAAcIU6sQX6YMpVQID3ze1SwEMWi3GJP7mrlosgkBBwoBSw&S=AQAAAtCuUt7EfsPszBLTpkM3Y2I; B=5998ckpfh1aqj&b=4&d=mwq7xr9tYFrZyzUak_Gb&s=up&i=fN7VLAQxaLcYk_uauWiy; GUC=AQEAAgJfewtgX0IbjwRZ; APID=1A9a944132-c795-11ea-89b6-1256e9c0edc4; SPTB=d=lQqud7manv2z1qws8sH2FbWi4iShZQ.8Y5298HGnLL.CXkV5dCqeHdpZasQPGXE0wFc.xUww6Tg.x6OAzBu05qBV.3gSBGuxtUSNbyjOIDU-&v=1; cmp=t=1602093624&j=0; A1S=d=AQABBFOrEF8CEMP-aWV8KHc-RRsdjqYMpVQFEgEAAgILe19fYB4Ab2UB_SMAAAcIU6sQX6YMpVQID3ze1SwEMWi3GJP7mrlosgkBBwoBSw&S=AQAAAtCuUt7EfsPszBLTpkM3Y2I&j=WORLD; APIDTS=1602093696"

scrapYahooProjection <- function(.week, .yahooCokies){

  scrp_df <- tibble(
    pos = c("O","K","DEF"),
    size= c(1025, 50, 25)
  ) %>%
    split(1:nrow(.)) %>%
    map_df(function(.position, .week, .cookies){
      .scrapYahooPosition(.position$pos, .position$size, .week=.week, .cookies = .cookies)
    }, .week=.week, .cookies=.yahooCokies) %>%
    select(-forecast, -na, -injuryStatus)
  
  ffa_columns = tibble(
    colName = names(projection_sources[["Yahoo"]]$stat_cols),
    shortName = projection_sources[["Yahoo"]]$stat_cols,
    janName   = make_clean_names(shortName)
  )
  
  scrp <- scrp_df %>%
    mutate(trends_percent_rostered=as.numeric(str_remove_all(trends_percent_rostered,"%"))/100) %>%
    mutate(across(c(gp:misc_blk_kick), as.numeric)) %>%
    pivot_longer(cols = -c(src_id, player, position, team), names_to="janName", values_to="value") %>%
    inner_join(ffa_columns, by="janName") %>%
    pivot_wider(id_cols=c(src_id, player, position, team), names_from=colName, values_from=value) %>%
    mutate(data_src="Yahoo") %>%
    select(data_src, everything())
  
  ## IDS do FFA
  # carregando tabelas de "de para" de IDs de Jogadores
  load("../ffanalytics/R/sysdata.rda") # <<- Players IDs !!!
  
  # miss_ids <- readRDS("./data/playerIds_not_mapped.rds") %>% 
  #   mutate( 
  #     id = as.character(id), 
  #     nfl_id = as.character(nfl_id)
  #   )
  # 
  # ffa_player_ids <- player_ids %>%
  #   # completa a tabela de mapeamento de projecoes do ffanalytics
  #   bind_rows(miss_ids) %>% 
  #   as_tibble() %>% 
  #   select(id, stats_id)
  
  scrp %>% 
    left_join(player_ids, by=c("src_id"="stats_id")) %>%
    mutate(position = if_else(position=="DEF","DST",position)) %>% 
    select(data_src, id, src_id, player, position, team, everything()) %>% 
    filter(!is.na(id)) %>% 
    return()
  
}

