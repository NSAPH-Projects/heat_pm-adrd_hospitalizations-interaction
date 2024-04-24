# Required libraries
library(data.table)
library(lubridate)
library(weathermetrics)
library(dplyr)
library(fst)
library(tidyverse)

setwd("~/heat_pm-adrd_hospitalizations-interaction/code")

# Load cohort and create variable year
load("../data/scratch/final-cohort_before_exposure.RData")

# Merge exposure
load("../data/scratch/exposure_dat.Rdata")
data = denom_merged %>%
  rename(date = week_days) %>%
  mutate(zip = sprintf("%05d", zip))%>%
  left_join(exposure_dat, by=c('zip', 'date')) 

# Remove patients with NA exposures
ids_prob = data$QID[!complete.cases(data[,c("pm25", "no2", "ozone", "heat_index")])]
data = data[!data$QID %in% ids_prob,]

# Remove data with heat index below 0 C degrees

data = as.data.table(data)
prob_temp = data[heat_index < 0]
prob_ID = unique(prob_temp$QID)

data = data[!data$QID %in% prob_ID,]

length(unique(data$QID))

save(data, file = "../data/scratch/case-crossover-exposure.Rdata")


