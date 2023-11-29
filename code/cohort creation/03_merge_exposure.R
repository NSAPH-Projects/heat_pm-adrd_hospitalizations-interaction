# Required libraries
library(data.table)
library(lubridate)
library(weathermetrics)
library(dplyr)
library(fst)
setwd("~/heat_pm-adrd_hospitalizations-interaction/code")

# Load cohort and create variable year
load("../data/scratch/final-cohort_before_exposure.RData")
final_cohort = final_cohort %>%
  mutate(year = year(admission))

# Load denom files
dir <- "/n/dominici_nsaph_l3/Lab/projects/analytic/denom_by_year/"
files <- list.files(dir, pattern = ".fst", full.names = TRUE)
cols <- c("zip", "year", "qid", "sex", "race", "age", "dual")

denom <- as.data.frame(rbindlist(lapply(files,
                                        read_fst,
                                        columns = cols,
                                        as.data.table = TRUE)))
# Merge cohort and denom data
denom_merged = merge(final_cohort, denom, 
      by.x = c("QID", "year"), by.y = c("qid", "year"))

# Merge exposure
load("../data/scratch/exposure_dat.Rdata")
data = denom_merged %>%
  rename(date = week_days) %>%
  mutate(zip = sprintf("%05d", zip))%>%
  left_join(exposure_dat, by=c('zip', 'date')) 

# Remove patients with NA exposures
ids_prob = data$QID[!complete.cases(data[,c("pm25", "no2", "ozone", "heat_index")])]
data = data[!data$QID %in% ids_prob,]

save(data, file = "../data/scratch/case-crossover-exposure.Rdata")


# ADJUST: remove data with temperature below 0 C degrees

load("../data/scratch/case-crossover-exposure.Rdata")

data[, max_temp_C:=max_temp-273.15]

prob_temp = data[max_temp_C < 0]
prob_ID = unique(prob_temp$QID)

data = data[!data$QID %in% prob_ID,]

save(data, file = "../data/scratch/case-crossover-exposure.Rdata")














######### TRASH

# Check zipcodes
data[,c("year", "zipcode_R", "zip")]
str(denom_merged)

all(denom_merged$zipcode_R == denom_merged$zip)

zip_cohort = unique(sprintf("%05d", as.integer(denom_merged$zipcode_R)))
zip_denom = unique(denom_merged$zip) # unique(sprintf("%05d", denom_merged$zip))
zip_heat = unique(heat_index$zip)
zip_pm = unique(exposure_dat$zip)


# Denom zip in exposure zip
zip_denom[!zip_denom %in%zip_heat]
length(zip_denom[!zip_denom %in%zip_heat])
length(zip_denom[!zip_denom %in%zip_heat])/ length(zip_denom)

zip_denom[!zip_denom %in%zip_pm]
length(zip_denom[!zip_denom %in%zip_pm])
length(zip_denom[!zip_denom %in%zip_pm])/ length(zip_denom)


# Cohort zip in exposure zip
zip_cohort[!zip_cohort %in%zip_heat]
length(zip_cohort[!zip_cohort %in%zip_heat])
length(zip_cohort[!zip_cohort %in%zip_heat])/ length(zip_cohort)

zip_cohort[!zip_cohort %in%zip_pm]
length(zip_cohort[!zip_cohort %in%zip_pm])
length(zip_cohort[!zip_cohort %in%zip_pm])/ length(zip_cohort)



##### Merge exposure based on denom zips
denom_merged = denom_merged %>%
  rename(date = week_days) %>%
  mutate(zip = sprintf("%05d", zip))%>%
  left_join(exposure_dat[,c("zip", "pm25", "date")], by=c('zip', 'date')) %>%
  left_join(heat_index, by=c('zip', 'date')) 

summary(denom_merged$pm25)
sum(is.na(denom_merged$pm25))
sum(is.na(denom_merged$heat_index))

denom_merged[is.na(denom_merged$pm25),]
denom_merged[is.na(denom_merged$heat_index),]



################## OLD
# Merge PM2.5
final_cohort = final_cohort %>%
  mutate(zip = sprintf("%05d", zipcode_R))%>%
  select(-zipcode_R)%>%
  left_join(exposure_dat[,c("ZIP", "pm25", "date")], by=c('zip'='ZIP', 'week_days'='date')) 

head(final_cohort)
sum(is.na(final_cohort$pm25))/nrow(final_cohort) # NAs in the zipcodes 0.5383421

# Ratio of problematic zipcode
prob_zip = unique(final_cohort$zip[is.na(final_cohort$pm25)])
length(prob_zip)/length(unique(final_cohort$zip)) # 0.5582317

# Ratio of patients living in the problematic zip codes
patients_probzip = unique(final_cohort$QID[final_cohort$zip %in% prob_zip])
length(patients_probzip)/length(unique(final_cohort$QID)) # 0.5510391

# Merge heat_index
final_cohort = final_cohort %>%
  mutate(zip = sprintf("%05d", zipcode_R))%>%
  select(-zipcode_R)%>%
  left_join(heat_index, by=c('zip'='zip', 'week_days'='date')) 

sum(is.na(final_cohort))
head(final_cohort)

sum(is.na(final_cohort$heat_index))/nrow(final_cohort) # 0.5525144
sum(is.na(final_cohort$min_humid))/nrow(final_cohort) # 0.5525144
sum(is.na(final_cohort$max_temp))/nrow(final_cohort) # 0.5525144

# Ratio of problematic zipcode
prob_zip_heat = unique(final_cohort$zip[is.na(final_cohort$heat_index)])
prob_zip_humid = unique(final_cohort$zip[is.na(final_cohort$min_humid)])
prob_zip_temp = unique(final_cohort$zip[is.na(final_cohort$max_temp)])

all(prob_zip_heat == prob_zip_humid)
all(prob_zip_heat == prob_zip_temp)
all(prob_zip_temp == prob_zip_humid)

length(prob_zip_heat)/length(unique(final_cohort$zip)) # 0.5754024

# Ratio of patients living in the problematic zip codes
patients_probzip = unique(final_cohort$QID[final_cohort$zip %in% prob_zip_heat])
length(patients_probzip)/length(unique(final_cohort$QID))


