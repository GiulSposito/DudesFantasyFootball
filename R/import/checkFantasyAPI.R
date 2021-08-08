library(httr)
library(glue)

# checkFantasyAPI_v2020 <- function(.authToken, .leagueId,.week){
# 
#   # obtem os matchups
#   url.matchups <- "http://api.fantasy.nfl.com/v2/league/matchups?appKey=internalemailuse&leagueId={.leagueId}&week={.week}&format=json&authToken={.authToken}"
#   
#   # faz a chamada na api
#   resp <- httr::GET(glue(url.matchups, week=.week))
#   
#   return(resp$status_code==200)
#   
# }

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

# authHeader="eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJjbGllbnRJZCI6ImFhYTE5MzY1LTE5NjQtNGRiYi05NjQ3LWNmZDZiYzc1NWZmMyIsImRtYUNvZGUiOiI3NjAwMSIsImlzcyI6Ik5GTCIsImRldmljZUlkIjoiMGZjMjQxMWEtZmVhMC00NDE2LWFhNDctOGJlMWQxY2UwM2FhIiwicHJvZHVjdE5hbWUiOiJGQU5UQVNZIiwiZGV2aWNlSW5mbyI6ImV5SnRiMlJsYkNJNkltUmxjMnQwYjNBaUxDSjJaWEp6YVc5dUlqb2lRMmh5YjIxbElpd2liM05PWVcxbElqb2lWMmx1Wkc5M2N5SXNJbTl6Vm1WeWMybHZiaUk2SWpFd0luMD0iLCJ1aWQiOiI5NDBhMmQ3MWFjZGY4M2RmNGIxODRhOTA5ZDhhMTU0MSIsInBsYW5zIjpbeyJzb3VyY2UiOiJORkwiLCJwbGFuIjoiZnJlZSIsInRyaWFsIjoiZmFsc2UiLCJzdGF0dXMiOiJBQ1RJVkUiLCJleHBpcmF0aW9uRGF0ZSI6IjIwMjItMDgtMDgifV0sImNvdW50cnlDb2RlIjoiQlIiLCJjZWxsdWxhciI6ZmFsc2UsImJyb3dzZXIiOiJDaHJvbWUiLCJEaXNwbGF5TmFtZSI6IkZBTlRBU1lfV0VCIiwibHVyYUFwcEtFeSI6IlNaczU3ZEJHUnhiTDcyOGxWcDdEWVEiLCJuZXR3b3JrVHlwZSI6Im90aGVyIiwiZXhwIjoxNjI4NDQxNjk3LCJOb3RlcyI6IiJ9.lCBGdU0iuAzJPQjgjmoaqNnvMEZh7xcSVbKNyWqNrcg"
# leagueId=3940933
# week=1
# # 
# checkFantasyAPI(authHeader, leagueId, week)
# 
# config$authHeader==authHeader
