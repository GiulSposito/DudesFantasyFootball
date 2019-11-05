# make sure of working dir because run as a job change it :(
setwd(normalizePath(rprojroot::find_rstudio_root_file()))
blogdown::serve_site()

