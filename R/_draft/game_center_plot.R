library(tidyverse)
library(glue)

.week <- 3
.prefix <- "preSNF"

sim <- readRDS(glue("./data/simulation_v5_week{.week}_{.prefix}.rds"))

# game summary in long form
teams_sims <- bind_rows(
  select(sim$matchup_sim, teamId=awayTeam.teamId, winProb=awayTeam.winProb, winProb.org=awayTeam.winProb.org, pred.Pts=awayTeam.totalPts, sim.pts=awayTeam.simulation, sim.pts.org=awayTeam.simulation.org),
  select(sim$matchup_sim, teamId=homeTeam.teamId, winProb=homeTeam.winProb, winProb.org=homeTeam.winProb.org, pred.Pts=homeTeam.totalPts, sim.pts=awayTeam.simulation, sim.pts.org=homeTeam.simulation.org)
) %>% mutate(
  pred.Pts.org = map_dbl(sim.pts.org, median, na.rm=T)
)

# obtem nome e pontuacao dos times
teams_stats <- sim$teams %>% 
  unnest( rosters ) %>% 
  filter( rosterSlotId < 20 ) %>%
  select( teamId, playerId, isEditable ) %>% 
  inner_join( sim$players_stats, by = "playerId" ) %>% 
  unnest( weekPts ) %>% 
  filter( week==.week ) %>% 
  select( teamId, playerId, isEditable, weekPts ) %>% 
  group_by( teamId ) %>% 
  summarise(
    players = n(),
    played  = sum(!isEditable),
    points = sum(weekPts, na.rm = T)
  )

teams <- sim$teams %>% 
  select(teamId, name, imageUrl, rank) %>% 
  inner_join(teams_sims, by = "teamId") %>% 
  inner_join(teams_stats, by = "teamId")


games_summary <- sim$matchup_sim %>% 
  inner_join(set_names(teams, paste0("away.",names(teams))), by=c("awayTeam.teamId"="away.teamId")) %>% 
  inner_join(set_names(teams, paste0("home.",names(teams))), by=c("homeTeam.teamId"="home.teamId")) %>% 
  mutate(game=paste0(away.name, " @ ", home.name)) %>% 
  mutate( away.nickname = gsub("([a-zA-Z\']+ )?", "", away.name),
          home.nickname = gsub("([a-zA-Z\']+ )?", "", home.name)) %>% 
  mutate( game.nickname = paste0(away.nickname, " @ ", home.nickname))

table_html_template <- "<table><thead><tr><th>{away.img}</th><th>{away.nickname}</th><th>{away.points}</th><th>@</th><th>{home.points}</th><th>{home.nickname}</th><th>{home.img}</th></tr></thead><tbody><tr><td>{away.winProb}</td><td>{away.remaining}</td><td>{away.pred.Pts}</td><td></td><td>{home.pred.Pts}</td><td>{home.remaining}</td><td>{home.winProb}</td></tr></tbody></table>"
table_htmlcss_template <- "<table style='border-collapse:collapse;border-spacing:0' class='tg'><thead><tr><th style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:15px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal'>{away.img}</th><th style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:15px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:right;vertical-align:top;word-break:normal'>{away.nickname}</th><th style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:15px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:right;vertical-align:top;word-break:normal'><span style='font-weight:bold'>{away.points}</span></th><th style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:15px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal'>@</th><th style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:15px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal'><span style='font-weight:bold'>{home.points}</span></th><th style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:15px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal'>{home.nickname}</th><th style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:15px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal'>{home.img}</th></tr></thead><tbody><tr><td style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:12px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal'>{away.winProb}</td><td style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:12px;overflow:hidden;padding:10px 5px;text-align:right;vertical-align:top;word-break:normal'>{away.remaining}</td><td style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:12px;overflow:hidden;padding:10px 5px;text-align:right;vertical-align:top;word-break:normal'>{away.pred.Pts}</td><td style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:14px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal'></td><td style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:12px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal'>{home.pred.Pts}</td><td style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:12px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal'>{home.remaining}</td><td style='border-color:inherit;border-style:solid;border-width:1px;font-family:'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;;font-size:12px;overflow:hidden;padding:10px 5px;text-align:left;vertical-align:top;word-break:normal'>{home.winProb}</td></tr></tbody></table>"

game_center <- games_summary %>%
  select(away.imageUrl, away.nickname, away.points, home.points, home.nickname, home.imageUrl,
         away.winProb, away.played, away.players, away.pred.Pts, 
         home.winProb, home.played, home.players, home.pred.Pts) %>% 
  mutate(
    across(c(away.points, home.points, away.pred.Pts, home.pred.Pts), round, digits=1),
    across(c(away.winProb, home.winProb), function(.x){ paste0(round(100*.x),"%") }),
    away.img = glue("<img src='{away.imageUrl}' style='width:40px;height:40px' alt=''>"),
    home.img = glue("<img src='{home.imageUrl}' style='width:40px;height:40px' alt=''>"),
    away.remaining = glue("({away.played}/{away.players})"),
    home.remaining = glue("({home.played}/{home.players})")
  ) %>% 
  mutate(
    htmlTable = glue(table_html_template)
  )
  

game_center$htmlTable %>% write("./export/gamecenter.html")

-# 
# team_prob <- sim_summary %>%
#   select(teamId, teamName=name, winProb, imageUrl)
# 
# team_rem <- sim$teams %>% 
#   select(teamId, rosters, season.stats) %>% 
#   unnest(c(rosters, season.stats)) %>% 
#   filter(rosterSlotId < 20) %>% 
#   group_by(teamId, record) %>% 
#   summarise(teamRemaining=sum(isEditable), .groups="keep") %>% 
#   ungroup() %>% 
#   mutate(record = str_remove(record, "-0")) %>% 
#   select(teamId, record, teamRemaining) %>% 
#   inner_join(team_prob, by="teamId")
# 
# game_center <- games_summary %>% 
#   select(game.nickname, away.nickname, away.projPts, away.pts, away.teamId, away.name,
#          home.pts, home.projPts, home.nickname, home.teamId, home.name) %>% 
#   left_join(set_names(team_rem, paste0("home.",names(team_rem))), by = "home.teamId") %>% 
#   left_join(set_names(team_rem, paste0("away.",names(team_rem))), by = "away.teamId")
# 
# game_center 
