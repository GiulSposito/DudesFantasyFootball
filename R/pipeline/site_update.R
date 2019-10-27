# on a r session

setwd("C:\\Projects\\DudesFantasyFootball")
start <- Sys.time()
blogdown::serve_site()
print(Sys.time()-start)

# git bash terminal

cd /c/Projects/DudesFantasyFootball/
git add *
git commit -m "site update & publishing"
git push
exit

