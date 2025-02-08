library(here)
library(dplyr)
library(tidyr)
library(lubridate)

#load data ----

#create file path
root <- here()
seriea_23_24 <- here("data/raw/a_23-24.csv")

data <- read.csv(seriea_23_24)

#clean data ----
data_cleaning <- here("code/data_prep/source/data_cleaning.R")
source(data_cleaning)

#add new features ----
data_features <- here("code/data_prep/source/data_features.R")
source(data_features)

#save cleaned and processed dfs
save(data_23_24, file = paste0(root, "/data/clean/data__matches_23_24"))
save(data_bet_23_24, file = paste0(root, "/data/clean/data_sportsbook_23_24"))
save(data_aggregate, file = paste0(root, "/data/processed/data_aggregated_matches_23_24"))
