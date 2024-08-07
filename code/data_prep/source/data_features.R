#add new features to data ----

data_aggregate <- data_23_24 %>%
  #combine home and away into one column
  pivot_longer(cols = c(home, away), names_to = "location", values_to = "team") %>%
  #indicate team result based on location
  mutate(win = case_when(
    (FTR == "H" & location == "home") ~ 1,
    (FTR == "A" & location == "away") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(loss = case_when(
    (FTR == "A" & location == "home") ~ 1,
    (FTR == "H" & location == "away") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(draw = case_when(
    (FTR == "D") ~ 1,
    TRUE ~ 0
  )) %>%
  #add goals scored based on location
  mutate(num_goals = case_when(
    location == "home" ~ FTHG,
    location == "away" ~ FTAG
  )) %>%
  #add goals allowed based on location
  mutate(num_goals_allowed = case_when(
    location == "home" ~ FTAG,
    location == "away" ~ FTHG
  )) %>%
  #turn into aggregate data
  group_by(team) %>%
  summarise(
    games_played = n(),
    #add match result info
    win_pct = (sum(win) / n()) * 100,
    wins = sum(win),
    losses = sum(loss),
    draws = sum(draw),
    #add goal info
    goals = sum(num_goals),
    goals_allowed = sum(num_goals_allowed),
    avg_goals = mean(num_goals, na.rm = TRUE),
    avg_goals_allowed = mean(num_goals_allowed, na.rm = TRUE)
  )


#add home/away performance: home win rate, away win rate, home/away goal differential
#add recent form: last 5 matches performance, current winning/losing/unbeaten streak
#add head-to-head record: historical and recent performance against opponent
#add key player performance: top scorer stats, key passes/assists, goalkeeper performance
#add recent player performance
#add league position