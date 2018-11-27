library(ffanalytics)
library(glue)
source("./R/_draft/score_settings.R")

# semana para usar na projecao
.week <- 13

# faz o scraping de projeção dos sites
scrap <- scrape_data(pos = c("QB", "RB", "WR", "TE", "DST", "K"),
                        season = 2018, 
                        week = .week)

# salva scrap da semana
scrap %>% saveRDS(glue("./data/week{.week}_scrap.rds"))

# faz a tabela de projeção de resultados
players.proj <- projections_table(scrap, scoring_rules = dudes.score.settings) 

players.proj %>% 
  filter(avg_type=="weighted") %>%
  filter(pos=="K") %>%
  select(pos,id,avg_type ,pos_rank, tier) %>% 
  distinct() %>% 
  arrange(id) -> kickers.attrib

scrap$K %>% 
  as.tibble() %>% 
  select(id, site_pts) %>% 
  group_by(id) %>% 
  summarise(
    points   = median(site_pts, na.rm = T),
    drop_off = NA,
    sd_pts   = sd(site_pts, na.rm = T),
    floor    = min(site_pts, na.rm = T),
    ceiling  = max(site_pts, na.rm = T)
  ) %>% 
  arrange(id) -> kickers.points

kickers.attrib %>% 
  inner_join(kickers.points, by = "id") %>% 
  bind_rows(players.proj %>% filter(avg_type=="weighted",pos!="K")) %>% 
  arrange(id) %>%
  add_player_info() %>% 
  mutate(id=as.integer(id)) -> projections

# matchups and rosters (nfl)
source("./R/tidy/matchups.R")
teams <- readRDS(glue("./data/week{.week}_matchups_json.rds")) %>% 
  extractTeams() %>% 
  mutate( 
    home.roster = map(home.roster, nfl2ffa, .ids=.players_id), # to ffa ids
    away.roster = map(away.roster, nfl2ffa, .ids=.players_id)  # to ffa ids
  )

# tydi teams
c("home","away") %>% 
  map(
    function(.prefix,.teams) {
      .teams %>% 
        select(starts_with(.prefix)) %>% 
        set_names(gsub(pattern=paste0(.prefix,"\\."),replacement = "",x=names(.))) %>%
        select(-pts) %>%
        rename(teamName=name) %>% 
        return()
    },
    .teams=teams
  ) %>% 
  bind_rows() %>% 
  unnest(roster) %>% 
  select(id, fantasy.team = teamName) -> players.team

players.team %>%
  right_join(projections, by = "id") %>%
  mutate(
    fantasy.team = gsub("([a-zA-Z\']+ )?", "", fantasy.team),
    fantasy.team = case_when(is.na(fantasy.team) ~ "*FreeAgent",
                             TRUE ~ fantasy.team)
  ) -> week.projections

saveRDS(week.projections, glue("./data/week{.week}_players_projections.rds"))
