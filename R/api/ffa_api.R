library(httr)
library(jsonlite)
library(purrr)

# Fantasy Football API Core GET
ffa_api_endpoint <- function(.url, .path, .query, .auth){
  
  # always return json
  .query = append(.query, list("format"="json"))
  
  # build full url and invokes
  url <- modify_url(url=.url, path=.path, query = .query)
  
  # user agent
  ua <- user_agent("dudes ffa")
  
  cat(url)
  cat("\n")
  cat(str(.query))
  
  # invoke
  resp <- GET(url,ua,
              add_headers(Authorization=.auth))
  
  # check formation type
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  # parse
  parsed <- fromJSON(content(resp,"text"))  
  
  # error handling
  if (http_error(resp)) {
    cat("http error: ", status_code(resp), "\n")
    cat(str(parsed))
    stop(
      sprintf(
        "Fantasy API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  # S3 return object
  structure(
    list(
      content  = parsed, 
      path     = .path,
      response = resp,
      status   = status_code( resp ),
      success  = !http_error(resp)
    ),
    class = "ffa_api"
  )
  
}


ffa_api <- function(.path, .query, .auth) ffa_api_endpoint("https://api.fantasy.nfl.com/", .path, .query, .auth)

# S3 class = "ffa_api" print
print.ffa_api <- function(x, ...) {
  cat("<FantasyApi ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}
