library(ffanalytics)
library(yaml)

# How to win a snake draft

# First, we use a script to scrape player’s projected points from numerous
# sources using R. 
scrp_df <- scrape_data(
  src = c("CBS", "FantasyPros", "FFToday", "FantasySharks"),
  pos = c("QB", "RB", "WR", "TE", "K", "DST"),
  season = 2020,
  week = 0
)


# Second, based on the user’s league scoring settings, we calculate players’
# projections using an average of the analysts’ projections (by default, the
# sources are weighted according to historical accuracy)
proj_df <- projections_table(scrp_df, scoring_rules = read_yaml("./config/score_settings.yml"))

# Third, we calculate the value of each player over a typical replacement player
# at his position (VOR) to determine player rankings. 


# In addition, we calculate players’ risk levels, as defined by the average of:
# 1) injury risk from Sports Injury Predictor, and 
# 2) the standard deviation of the players’ projected points and rankings across analysts.
pts_df <- proj_df %>% 
  add_ecr() %>% 
  add_risk()
  # add_adp() 
  # add_aav()

players_projection <- proj_df %>% 
  add_player_info()
