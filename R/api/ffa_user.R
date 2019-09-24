source("./R/api/ffa_api.R")

# return the user id, leagues and teams of a 'authToken'
ffa_user_leagues <- function(.authToken){
  ffa_api(
    .path = "v1/user/leagues",
    .query = list( "authToken"=.authToken)
  )
}

