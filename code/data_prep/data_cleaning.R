#examine missingness ----

missingness <- data %>%
  rowwise() %>%
  mutate(missing_count = sum(across(everything(), ~is.na(.))),
         #calculate percentage of missing columns
         missing_percent = (missing_count / ncol(across(everything()))) * 100
  ) %>%
  ungroup()

#create report to examine missingness
missingness_report <- missingness %>%
  select(Date, Time, HomeTeam, AwayTeam, missing_count, missing_percent) %>%
  filter(missing_count > 0)

#note columns with high missingness
column_missing_percentage <- data %>%
  summarise_all(~mean(is.na(.)) * 100) %>%
  pivot_longer(cols = everything(),
               names_to = "column",
               values_to = "missing_percent")

c("BWH","BWD","BWA","IWH","IWD","IWA","BWCH","BWCD","BWCA","IWCH","IWCD","IWCA") #columns with missing values

#review outliers ----

#verify columns FTR and HTR only contain values of H, D, or A
distinct_FTR <- data %>%
  distinct(FTR)
distinct_HTR <- data %>%
  distinct(FTR)

expected_result_values <- c("A","H","D")

  #check for unexpected values in distinct_FTR
if (any(!distinct_FTR$FTR %in% expected_result_values)) {
  warning("Unexpected value found in distinct_FTR!")
  print(distinct_FTR$FTR[!distinct_FTR$FTR %in% expected_result_values])
}

  #check for unexpected values in distinct_HTR
if (any(!distinct_HTR$HTR %in% expected_result_values)) {
  warning("Unexpected value found in distinct_HTR!")
  print(distinct_HTR$HTR[!distinct_HTR$HTR %in% expected_result_values])
}

#identify row indices for outliers where absolute z-score > 3
outliers_z <- data %>%
  summarise(across(where(is.numeric), ~list(
    outliers = which(abs(scale(.)) > 3)
  )))

#investigate outliers
outlier_rows <- data[c(3, 8, 259, 380),]

#format data ----

data_all <- data %>%
  #convert Date column to Date type
  mutate(MatchDate = dmy(Date)) %>%
  relocate(MatchDate, .after = Date) %>%
  select(-Date) %>%
  rename(Date = MatchDate) %>%
  #add matchday data
  mutate(
    MatchDay = case_when(
      Date >= as.Date("2023-08-19") & Date <= as.Date("2023-08-21") ~ 1,
      Date >= as.Date("2023-08-26") & Date <= as.Date("2023-08-28") ~ 2,
      Date >= as.Date("2023-09-01") & Date <= as.Date("2023-09-03") ~ 3,
      Date >= as.Date("2023-09-16") & Date <= as.Date("2023-09-18") ~ 4,
      Date >= as.Date("2023-09-22") & Date <= as.Date("2023-09-24") ~ 5,
      Date >= as.Date("2023-09-26") & Date <= as.Date("2023-09-28") ~ 6,
      Date >= as.Date("2023-09-30") & Date <= as.Date("2023-10-02") ~ 7,
      Date >= as.Date("2023-10-06") & Date <= as.Date("2023-10-08") ~ 8,
      Date >= as.Date("2023-10-21") & Date <= as.Date("2023-10-23") ~ 9,
      Date >= as.Date("2023-10-27") & Date <= as.Date("2023-10-30") ~ 10,
      Date >= as.Date("2023-11-03") & Date <= as.Date("2023-11-06") ~ 11,
      Date >= as.Date("2023-11-10") & Date <= as.Date("2023-11-12") ~ 12,
      Date >= as.Date("2023-11-25") & Date <= as.Date("2023-11-27") ~ 13,
      Date >= as.Date("2023-12-01") & Date <= as.Date("2023-12-04") ~ 14,
      Date >= as.Date("2023-12-08") & Date <= as.Date("2023-12-11") ~ 15,
      Date >= as.Date("2023-12-15") & Date <= as.Date("2023-12-18") ~ 16,
      Date >= as.Date("2023-12-22") & Date <= as.Date("2023-12-23") ~ 17,
      Date >= as.Date("2023-12-29") & Date <= as.Date("2023-12-30") ~ 18,
      Date >= as.Date("2024-01-05") & Date <= as.Date("2024-01-07") ~ 19,
      Date >= as.Date("2024-01-13") & Date <= as.Date("2024-01-16") ~ 20,
      Date %in% as.Date(c("2024-01-20","2024-01-21","2024-02-14","2024-02-22","2024-02-28")) ~ 21,
      Date >= as.Date("2024-01-26") & Date <= as.Date("2024-01-29") ~ 22,
      Date >= as.Date("2024-02-02") & Date <= as.Date("2024-02-05") ~ 23,
      Date >= as.Date("2024-02-09") & Date <= as.Date("2024-02-12") ~ 24,
      Date >= as.Date("2024-02-16") & Date <= as.Date("2024-02-18") ~ 25,
      Date >= as.Date("2024-02-23") & Date <= as.Date("2024-02-26") ~ 26,
      Date >= as.Date("2024-03-01") & Date <= as.Date("2024-03-04") ~ 27,
      Date >= as.Date("2024-03-08") & Date <= as.Date("2024-03-11") ~ 28,
      (Date >= as.Date("2024-03-15") & Date <= as.Date("2024-03-17")) | Date == as.Date("2024-06-02") ~ 29,
      Date >= as.Date("2024-03-30") & Date <= as.Date("2024-04-01") ~ 30,
      Date >= as.Date("2024-04-05") & Date <= as.Date("2024-04-08") ~ 31,
      (Date >= as.Date("2024-04-12") & Date <= as.Date("2024-04-15")) | Date == as.Date("2024-04-25") ~ 32,
      Date >= as.Date("2024-04-19") & Date <= as.Date("2024-04-22") ~ 33,
      Date >= as.Date("2024-04-26") & Date <= as.Date("2024-04-29") ~ 34, 
      Date >= as.Date("2024-05-03") & Date <= as.Date("2024-05-06") ~ 35,
      Date >= as.Date("2024-05-10") & Date <= as.Date("2024-05-13") ~ 36,
      Date >= as.Date("2024-05-17") & Date <= as.Date("2024-05-20") ~ 37, 
      Date >= as.Date("2024-05-23") & Date <= as.Date("2024-05-26") ~ 38,
      TRUE ~ 99
    )
  ) %>%
  relocate(MatchDay, .before = Date)

#separate data into match data and sportsbook data; rename columns

data_23_24 <- data_all %>%
  select(
    matchday = MatchDay,
    date = Date,
    time = Time,
    home = HomeTeam,
    away = AwayTeam,
    FTHG:AR
    )

data_bet_23_24 <- data_all %>%
  select(
    matchday = MatchDay,
    date = Date,
    time = Time,
    home = HomeTeam,
    away = AwayTeam,
    B365H:AvgA,
    B365_over2.5 = B365.2.5,
    B365_under2.5 = B365.2.5.1,
    P_over2.5 = P.2.5,
    P_under2.5 = P.2.5.1,
    Max_over2.5 = Max.2.5,
    Max_under2.5 = Max.2.5.1,
    Avg_over2.5 = Avg.2.5,
    Avg_under2.5 = Avg.2.5.1,
    AHh:AvgCA,
    B365_closing_over2.5 = B365C.2.5,
    B36_closing_under2.5 = B365C.2.5.1,
    P_closing_over2.5 = PC.2.5,
    P_closing_under2.5 = PC.2.5.1,
    Max_closing_over2.5 = MaxC.2.5,
    Max_closing_under2.5 = MaxC.2.5.1,
    Avg_closing_over2.5 = AvgC.2.5,
    Avg_closing_under2.5 = AvgC.2.5.1,
    AHCh:AvgCAHA
  )
