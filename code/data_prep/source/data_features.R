#add new features to data ----

data_aggregate <- data_23_24 %>%
  #combine home and away into one column
  pivot_longer(cols = c(home, away), names_to = "location", values_to = "Club") %>%
  #indicate club result based on location
  mutate(
    win = case_when(
    (FTR == "H" & location == "home") ~ 1,
    (FTR == "A" & location == "away") ~ 1,
    TRUE ~ 0
    ),
    loss = case_when(
    (FTR == "A" & location == "home") ~ 1,
    (FTR == "H" & location == "away") ~ 1,
    TRUE ~ 0
    ),
    draw = case_when(
    (FTR == "D") ~ 1,
    TRUE ~ 0
    ),
    #indicate home and away wins
    home_win = if_else(location == "home" & win == 1, 1, 0),
    away_win = if_else(location == "away" & win == 1, 1, 0),
    #add goals for and against based on location
    FTHGnum = as.numeric(as.character(FTHG)),
    FTAGnum = as.numeric(as.character(FTAG)),
    home_GF = case_when(
      location == "home" ~ FTHGnum,
      TRUE ~ 0
    ),
    away_GF = case_when(
      location == "away" ~ FTAGnum,
      TRUE ~ 0
    ),
    home_GA = case_when(
      location == "home" ~ FTAGnum,
      TRUE ~ 0
    ),
    away_GA = case_when(
      location == "away" ~ FTHGnum,
      TRUE ~ 0
    ),
    #add goal difference based on location
    goal_difference = case_when(
    location == "home" ~ home_GF - home_GA,
    location == "away" ~ away_GF - away_GA,
    TRUE ~ 0
    ),
    #add shots based on location
    HSnum = as.numeric(as.character(HS)),
    ASnum = as.numeric(as.character(AS)),
    home_shots = case_when(
      location == "home" ~ HSnum,
      TRUE ~ 0
    ),
    away_shots = case_when(
      location == "away" ~ ASnum,
      TRUE ~ 0
    ),
    #add shots on target based on location
    HSTnum = as.numeric(as.character(HST)),
    ASTnum = as.numeric(as.character(AST)),
    home_ST = case_when(
      location == "home" ~ HSTnum,
      TRUE ~ 0
    ),
    away_ST = case_when(
      location == "away" ~ ASTnum,
      TRUE ~ 0
    ),
    #add corners based on location
    HCnum = as.numeric(as.character(HC)),
    ACnum = as.numeric(as.character(AC)),
    home_C = case_when(
      location == "home" ~ HCnum,
      TRUE ~ 0
    ),
    away_C = case_when(
      location == "away" ~ ACnum,
      TRUE ~ 0
    )
  ) %>%
  #turn into aggregate data
  group_by(Club) %>%
  summarise(
    MP = n(), #matches played
    #add match result info
    WinPct = (sum(win) / n()) * 100,
    W = sum(win), #sum of wins
    D = sum(draw), #sum of draws
    L = sum(loss), #sum of losses
    #add goal info
    GF = sum(home_GF, na.rm = TRUE) + sum(away_GF, na.rm = TRUE), #all goals for
    GA = sum(home_GA, na.rm = TRUE) + sum(away_GA, na.rm = TRUE), #all goals against
    GD = GF - GA, #goal difference
    AvgGF = GF / MP,
    AvgGA = GA / MP,
    #add home/away performance
    HomeWinPct = (sum(home_win, na.rm = TRUE) / sum(location == "home")) * 100,
    AwayWinPct = (sum(away_win, na.rm = TRUE) / sum(location == "away")) * 100,
    #add home/away goal difference
    HomeGD = sum(home_GF, na.rm = TRUE) - sum(home_GA, na.rm = TRUE),
    AwayGD = sum(away_GF, na.rm = TRUE) - sum(away_GA, na.rm = TRUE),
    #add (home/away) shots
    Shots = sum(home_shots, na.rm = TRUE) + sum(away_shots, na.rm = TRUE),
    HomeShots = sum(home_shots, na.rm = TRUE),
    AwayShots = sum(away_shots, na.rm = TRUE),
    AvgShots = Shots / MP,
    #add (home/away) shots on target
    ST = sum(home_ST, na.rm = TRUE) + sum(away_ST, na.rm = TRUE),
    HST = sum(home_ST, na.rm = TRUE),
    AST = sum(away_ST, na.rm = TRUE),
    AvgST = ST / MP,
    #add (home/away) corners
    Crnr = sum(home_C, na.rm = TRUE) + sum(away_C, na.rm = TRUE),
    HC = sum(home_C, na.rm = TRUE),
    AC = sum(away_C, na.rm = TRUE),
    AvgCrnr = Crnr / MP
  )


#add recent form: last 5 matches performance, current winning/losing/unbeaten streak
#add head-to-head record: historical and recent performance against opponent
#add key player performance: top scorer stats, key passes/assists, goalkeeper performance
#add recent player performance
#add league position