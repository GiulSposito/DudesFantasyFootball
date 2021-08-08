library(httr)
library(glue)

checkFantasyAPI <- function(.authHeader, .leagueId, .week){
  
  # obtem os matchups
  url.matchups <- "http://api.fantasy.nfl.com/v2/league/matchups?appKey=internalemailuse&leagueId={.leagueId}&week={.week}&format=json"
  
  # faz a chamada na api
  resp <- httr::GET(
    glue(url.matchups, week=.week),
    add_headers(Authorization=.authHeader)
  )
  
  return(resp$status_code==200)
  
}
