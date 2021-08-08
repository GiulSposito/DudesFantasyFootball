library(httr)
library(glue)


# obtem os matchups
url.matchups <- "https://api2.fantasy.nfl.com/v2/user/leagues?appKey=internalemailuse"
url.players <- "https://api.fantasy.nfl.com/v2/league/players?appKey=internalemailuse&count=20&format=json&leagueId=3940933"

# faz a chamada na api
resp <- httr::GET(
    glue(url.players),
    add_headers(Authorization="eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJjbGllbnRJZCI6ImFhYTE5MzY1LTE5NjQtNGRiYi05NjQ3LWNmZDZiYzc1NWZmMyIsImRtYUNvZGUiOiI3NjAwMSIsImlzcyI6Ik5GTCIsImRldmljZUlkIjoiMGZjMjQxMWEtZmVhMC00NDE2LWFhNDctOGJlMWQxY2UwM2FhIiwicHJvZHVjdE5hbWUiOiJGQU5UQVNZIiwiZGV2aWNlSW5mbyI6ImV5SnRiMlJsYkNJNkltUmxjMnQwYjNBaUxDSjJaWEp6YVc5dUlqb2lRMmh5YjIxbElpd2liM05PWVcxbElqb2lWMmx1Wkc5M2N5SXNJbTl6Vm1WeWMybHZiaUk2SWpFd0luMD0iLCJ1aWQiOiI5NDBhMmQ3MWFjZGY4M2RmNGIxODRhOTA5ZDhhMTU0MSIsInBsYW5zIjpbeyJzb3VyY2UiOiJORkwiLCJwbGFuIjoiZnJlZSIsInRyaWFsIjoiZmFsc2UiLCJzdGF0dXMiOiJBQ1RJVkUiLCJleHBpcmF0aW9uRGF0ZSI6IjIwMjItMDgtMDgifV0sImNvdW50cnlDb2RlIjoiQlIiLCJjZWxsdWxhciI6ZmFsc2UsImJyb3dzZXIiOiJDaHJvbWUiLCJEaXNwbGF5TmFtZSI6IkZBTlRBU1lfV0VCIiwibHVyYUFwcEtFeSI6IlNaczU3ZEJHUnhiTDcyOGxWcDdEWVEiLCJuZXR3b3JrVHlwZSI6Im90aGVyIiwiZXhwIjoxNjI4NDQxNjk3LCJOb3RlcyI6IiJ9.lCBGdU0iuAzJPQjgjmoaqNnvMEZh7xcSVbKNyWqNrcg")
  )

resp$status_code
content(resp, as="text")
  


https://api.fantasy.nfl.com/v2/league/players?appKey=xbifvuekvkjhg0xg&authToken=_authToken_&count=20&format=json&leagueId=3071891&offset=0&playerStatus=available&position=O&sort=%7B%22week%22%3A%221%22%2C%22season%22%3A%222015%22%2C%22type%22%3A%22researchStats%22%2C%22statId%22%3A%22percentRostered%22%7D&stats=%5B%7B%22type%22%3A%22researchStats%22%2C%22season%22%3A%222015%22%2C%22week%22%3A%221%22%7D%2C%7B%22type%22%3A%22ranks%22%2C%22season%22%3A%222015%22%2C%22week%22%3A%221%22%7D%2C%7B%22type%22%3A%22stats%22%2C%22season%22%3A%222015%22%2C%22week%22%3A%221%22%7D%2C%7B%22type%22%3A%22stats%22%2C%22season%22%3A%222015%22%7D%2C%7B%22type%22%3A%22projectedStats%22%2C%22season%22%3A%222015%22%2C%22week%22%3A%221%22%7D%2C%7B%22type%22%3A%22nflGames%22%2C%22season%22%3A%222015%22%2C%22week%22%3A%221%22%7D%5D
