library(ffanalytics)
library(yaml)

# How to win a snake draft

# references
# https://fantasyfootballanalytics.net/2013/09/win-your-fantasy-football-snake-draft.html
# https://fantasyfootballanalytics.net/2013/04/win-your-snake-draft-calculating-value.html

##### SCRAP AND CALCULATIONS

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
# Third, we calculate the value of each player over a typical replacement player
# at his position (VOR) to determine player rankings. 
proj_df <- projections_table(scrp_df, scoring_rules = read_yaml("./config/score_settings.yml"))


# In addition, we calculate players’ risk levels, as defined by the average of:
#  1) injury risk from Sports Injury Predictor, and 
#  2) the standard deviation of the players’ projected points and rankings across analysts.
# We also display the “dropoff” in projected points for the next best 2 players at the same position.  
# If a player has a high dropoff, you should consider targeting him because that position drops in value very quickly.  
pts_df <- proj_df %>% 
  add_ecr() %>% 
  add_risk() %>% 
  # fix risk calculations
  # because as a matrix note a column
  mutate( risk = .$risk[,1])  

# add players info
players_projection <- pts_df %>% 
  add_player_info()


##### DRAFT RANKING

# The goal is to pick the remaining players at each pick that maximize the team’s 
# sum of projected points for the starting lineup, while minimizing the downside risk of the starters.

# A Harvard study found that the best drafting style was one that: 
#  1) considered a player’s projected points relative to that of a typical
#     replacement player (rather than a player’s number of projected points in absolute terms), and
#  2) maximized this value for each position in the starting lineup before drafting bench players

# I will refer to requirement 1 as value-based drafting, which relies on a metric known as value over replacement player (VORP).
# I will refer to requirement 2 as starting lineup first.

# When drafting players, it makes sense to draft your offensive starters first before you draft any bench players.
# Moreover, depending on your league settings, it’s generally best to wait on Kickers and Defenses until later rounds
# because they have lower predictability and score fewer points, on average, than other positions


# 1. Draft your offensive starting lineup first before drafting bench players.  
#    Save Kickers and Defenses for later rounds because they are less predictable 
#    and score fewer points.

# 2. For starters, pick the player with the highest value (over a typical replacement)
#    who has a low risk and a high floor.  Bench players only benefit your team
#    if a starter gets injured or they outperform a starter.  

# statistics type
teams_n <- 10
choosed_avg_type <- "weighted"

# start players numbers (1 QB, 2 WR, 2 RB + 1 TE + 1 W/R) * team number
starters_n <- (1 + 2 + 2 + 1 + 1) * teams_n

starters <- players_projection %>% 
  filter( position %in% c("QB","WR","RB","TE"), avg_type==choosed_avg_type) %>% 
  arrange(desc(points_vor), risk, desc(floor)) %>%
  slice(1:starters_n)

# benchs slots = 4 | 1 QB, 1 WR, 1 RB, 1 TE
benchs_n <- 4 * teams_n

# 3. In a standard league, 1500 points would likely put you in the top 10% of teams.
#    Your goal is thus to maximize your starting lineup’s projected points.

# 4. For bench players, pick the player with the highest value (over a typical replacement)
#    who has a high ceiling.  That way, one or more of your bench players has a chance to 
#    outperform your starters or be a solid replacement if your starters get injured.
#    You should be willing to accept a higher risk for bench players than for starters.

benchers <- players_projection %>% 
  filter( position %in% c("QB","WR","RB","TE"), avg_type==choosed_avg_type) %>% 
  anti_join(startersby = c("id")) %>% 
  arrange(desc(points_vor), desc(ceiling)) %>% 
  slice(1:benchs_n)

# defense and kickers
dst_k_n <- 2 * teams_n

dst_k <- players_projection %>% 
  filter( position %in% c("DST","K"), avg_type==choosed_avg_type) %>% 
  arrange(desc(points_vor), risk, desc(floor)) %>%
  slice(1:dst_k_n)

# reserves (2 extra benchs)
reserves <- players_projection %>% 
  filter(position %in% c("WR","RB"), avg_type==choosed_avg_type) %>% 
  anti_join(starters, by="id") %>% # 1 QB, 2 WR, 2 RB, 1 TE, 1 W/R
  anti_join(benchers, by="id") %>% # 1 QB, 1 WR, 1 RB, 1 TE
  anti_join(dst_k,    by="id") %>% # 1 K, 1 DST
  arrange(desc(points_vor),desc(ceiling))

# Give players more weight if they have either:
#  - a high dropoff (the player is projected to score many more points than the next
#    best players at the same position), or
#  - a low ADP Difference score (i.e., the player tends to be under-valued compared 
#    to ADP and you might be able to draft him in a later round).
