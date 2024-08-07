library(here)
library(dplyr)
library(tidyr)
library(lubridate)

#load data ----

#create file path
seriea_23_24 <- here("data/a_23-24.csv")

data <- read.csv(seriea_23_24)

#clean data ----
data_cleaning <- here("code/data_prep/source/data_cleaning.R")
source(data_cleaning)

#add new features ----
data_features <- here("code/data_prep/source/data_features.R")
source(data_features)

