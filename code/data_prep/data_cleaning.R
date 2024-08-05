library(here)
library(dplyr)

#load data ----

#create file path
seriea_23_24 <- here("data/a_23-24.csv")

data <- read.csv(seriea_23_24)

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

#review outliers ----

#format data consistently ----