library(knitr)
library(markdown)
source("./R/import/checkFantasyAPI.R")
source("./R/import/import_matchups.R")
source("./R/simulation/points_simulation_v3.R")

week <- 12

checkFantasyAPI(week)

importMatchups(week) -> json

simulateGames(week)


rmarkdown::render(
  input = "./R/reports/dudes_simulation_v2.Rmd",
  output_file = "../../public/dudes_simulation_week12_posTNF",
  output_format = "html_document"
  )

?render
