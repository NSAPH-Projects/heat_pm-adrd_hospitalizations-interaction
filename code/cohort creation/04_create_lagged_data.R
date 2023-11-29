# Required libraries
library(data.table)
library(lubridate)
library(tidyverse)
library(weathermetrics)
library(dplyr)
library(fst)
setwd("~/heat_pm-adrd_hospitalizations-interaction/code")

# Exposures directory
dir_exposures <- "/n/dominici_lab_ro/lab/data/exposures/exposure/"
dir_temp <- "/n/dominici_nsaph_l3/Lab/data/gridmet_flat/"

# Load cohort to select just zipcodes of interest
load("../data/scratch/case-crossover-exposure.Rdata")
zips <- unique(data$zip)

# Daily air pollution data
year_range <- 2000:2016
month_range <- 1:12 

# PM 2.5
file <- "pm25/PM25_v2" # pm25/PM25_v2, no2/NO2_v2, ozone/O3_v2
pm25_dat <- rbindlist(lapply(year_range, function(y) {
  rbindlist(lapply(month_range, function(m) {
    rbindlist(lapply(1:days_in_month(ymd(paste0(y, "/", m, "/1"))), function(d) {
      dat <- readRDS(paste0(dir_exposures, file, "/daily/",
                            y, sprintf("%02d", m), sprintf("%02d", d), ".rds"))
      dat$date <- ymd(paste0(y, sprintf("%02d", m), sprintf("%02d", d)))
      dat
    }))
  }))
}))

pm25_dat = pm25_dat[ZIP %in% zips]
pm25_dat[order(ZIP, date), paste0("pm_lag_", 1:14) := shift(.SD, 1:14, type = "lag"), by = "ZIP", .SDcols = "pm25"]
pm25_dat = na.omit(pm25_dat)

# NO2
file <- "no2/NO2_v2" # pm25/PM25_v2, , ozone/O3_v2
no2_dat <- rbindlist(lapply(year_range, function(y) {
  rbindlist(lapply(month_range, function(m) {
    rbindlist(lapply(1:days_in_month(ymd(paste0(y, "/", m, "/1"))), function(d) {
      dat <- readRDS(paste0(dir_exposures, file, "/daily/",
                            y, sprintf("%02d", m), sprintf("%02d", d), ".rds"))
      dat$date <- ymd(paste0(y, sprintf("%02d", m), sprintf("%02d", d)))
      dat
    }))
  }))
}))
no2_dat = no2_dat[ZIP %in% zips]
no2_dat[order(ZIP, date), paste0("no2_lag_", 1:14) := shift(.SD, 1:14, type = "lag"), by = "ZIP", .SDcols = "no2"]
no2_dat = na.omit(no2_dat)

# O3
file <- "ozone/O3_v2" # pm25/PM25_v2, , ozone/O3_v2
O3_dat <- rbindlist(lapply(year_range, function(y) {
  rbindlist(lapply(month_range, function(m) {
    rbindlist(lapply(1:days_in_month(ymd(paste0(y, "/", m, "/1"))), function(d) {
      dat <- readRDS(paste0(dir_exposures, file, "/daily/",
                            y, sprintf("%02d", m), sprintf("%02d", d), ".rds"))
      dat$date <- ymd(paste0(y, sprintf("%02d", m), sprintf("%02d", d)))
      dat
    }))
  }))
}))

O3_dat = O3_dat[ZIP %in% zips]
O3_dat[order(ZIP, date), paste0("ozone_lag_", 1:14) := shift(.SD, 1:14, type = "lag"), by = "ZIP", .SDcols = "ozone"]
O3_dat = na.omit(O3_dat)


pm25_dat = pm25_dat %>%
  select(-STATE)
no2_dat = no2_dat %>%
  select(-STATE)
O3_dat = O3_dat %>%
  select(-STATE)

# Merge exposures
df_list <- list(pm25_dat, no2_dat, O3_dat)

exposure_dat = df_list %>% reduce(full_join, by=c("ZIP", "date"))
exposure_dat$ZIP = sprintf("%05d", as.integer(exposure_dat$ZIP)) 

# Daily max temperature, min humidity, heat index data
year_range <- 2000:2016

# load gridmet max temp
max_temp <- rbindlist(lapply(year_range, function(y) {
  load(paste0(dir_temp, 
              "maximum_air_temperature/", y,
              "_maximum_air_temperature_by_zip.RData"))
  df$zip <- rownames(df)
  setDT(df)
  max_temp <- melt(df, id.vars = "zip", variable.name = "date", value.name = "max_temp")
  max_temp[, date := ymd(date)]
  max_temp[, zip := sprintf("%05d", as.integer(zip))]
}))

# load gridmet min humidity
min_humid <- rbindlist(lapply(year_range, function(y) {
  load(paste0(dir_temp, 
              "minimum_relative_humidity/", y,
              "_minimum_relative_humidity_by_zip.RData"))
  df$zip <- rownames(df)
  setDT(df)
  min_humid <- melt(df, id.vars = "zip", variable.name = "date", value.name = "min_humid")
  min_humid[, date := ymd(date)]
  min_humid[, zip := sprintf("%05d", as.integer(zip))]
}))

min_humid$min_humid[min_humid$min_humid > 100] = 100

# calculate heat index
heat_index <- merge(max_temp, min_humid, by = c("zip", "date"))
heat_index[, heat_index := 
             heat.index(max_temp - 273.15, 
                        rh = min_humid,
                        temperature.metric = "celsius", 
                        output.metric = "celsius", round = 3)]

heat_index = heat_index[zip %in% zips]

heat_index[order(zip, date), paste0("min_humid_lag_", 1:14) := shift(.SD, 1:14, type = "lag"), by = "zip", .SDcols = "min_humid"]
heat_index[order(zip, date), paste0("max_temp_lag_", 1:14) := shift(.SD, 1:14, type = "lag"), by = "zip", .SDcols = "max_temp"]
heat_index[order(zip, date), paste0("heat_index_lag_", 1:14) := shift(.SD, 1:14, type = "lag"), by = "zip", .SDcols = "heat_index"]
heat_index = na.omit(heat_index)

# Merge 
exposure_dat = exposure_dat %>% 
  rename(zip = ZIP) %>%
  full_join(heat_index, by=c("zip", "date"))

save(exposure_dat, file = "../data/scratch/exposure_lagged_dat.Rdata")


# Merge with patient data
load("../data/scratch/exposure_lagged_dat.Rdata")
load("../data/scratch/case-crossover-exposure.Rdata")

data = data[,1:16] %>%
  left_join(exposure_dat, by=c('zip', 'date')) 

data = data%>% 
  rename(pm_lag_0 = pm25, 
         ozone_lag_0 = ozone,
         no2_lag_0 = no2,
         heat_index_lag_0 = heat_index,
         max_temp_lag_0 = max_temp,
         min_humid_lag_0 = min_humid)
save(data, file = "../data/scratch/cohort_lagged_dat.Rdata")

# ADJUST: remove data with temperature below 0 C degrees

load("../data/scratch/cohort_lagged_dat.Rdata")

data = data[!data$QID %in% prob_ID,]
length(unique(data$QID))

save(data, file = "../data/scratch/cohort_lagged_dat.Rdata")
