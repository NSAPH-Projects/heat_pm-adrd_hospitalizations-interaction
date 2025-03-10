library(fst)
library(data.table)
library(dplyr)
library(tidyverse)
library(magrittr)
library(timeperiodsR)
library(lubridate)
setwd("/n/dominici_nsaph_l3/Lab/projects/heat_pm-adrd_hospitalizations-interaction/code")

## Get data ----
# Get medpar admissions data
f <- list.files("/n/dominici_nsaph_l3/Lab/projects/analytic/admissions_by_year/",
                pattern = "\\.fst",
                full.names = TRUE)

myvars <- c("QID", "ADATE", "DDATE", "zipcode_R", "DIAG1", "DIAG2", "DIAG3", "DIAG4", "DIAG5",
            "DIAG6", "DIAG7", "DIAG8", "DIAG9", "DIAG10", "AGE", "Sex_gp", "Race_gp",
            "SSA_STATE_CD", "SSA_CNTY_CD", "PROV_NUM", "ADM_SOURCE", "ADM_TYPE", "Dual")   

medpar_admissions <- as.data.frame(rbindlist(lapply(f,
                                                    read_fst,
                                                    columns = myvars,
                                                    as.data.table = TRUE)))%>% 
  filter(AGE < 100)%>%
  mutate(QID = as.character(QID))%>%
  mutate(ADATE = as.Date(ADATE, "%d%b%Y"), DDATE = as.Date(DDATE, "%d%b%Y")) %>%
  filter(year(ADATE) > 1999) 


## Identify ADRD hospitalizations based on Moura/Festa's definition ----
# Change variables in character
codes_var = c("DIAG1", "DIAG2", "DIAG3", "DIAG4", "DIAG5", "DIAG6", "DIAG7", "DIAG8", "DIAG9", "DIAG10")
medpar_admissions[codes_var] <- lapply(medpar_admissions[codes_var], as.character) 

# ADRD codes (both ICD9 and ICD10)
ICD9 = c("3310", "33111", "33119", "3312", "3317", "2900", "29010", "29011", "29012", "29013", "29020", "29021", "2903", "29040", "29041", "29042", "29043", "2940", "29410", "29411", "29420", "29421", "2948", "797")

ICD10 = c("F0150", "F0151", "F0280", "F0281", "F0390", "G309", "G300", "G301", "G308", "G3101", "G3109", "G311", "G312", "R4181")

ADRD_codes = c(ICD9, ICD10)

# Identify and select ADRD hospitalization: 
# select patients with at least one qualifying claim among the first 10 billing codes
ADRD_hospitalization = medpar_admissions %>% 
  mutate(codes_num = rowSums(across(starts_with("DIAG"), ~`%in%`(.,ADRD_codes))))%>%
  filter(codes_num > 0 )
length(unique(ADRD_hospitalization$QID))

## Define first ADRD hospitalization ----
hosp_tab = data.table(ADRD_hospitalization)
first_ADRDhosp = as.data.frame(hosp_tab[hosp_tab[ , .I[which.min(ADATE)], by = QID]$V1])

# Save first ADRD hospitalization for every patient
first_ADRDhosp = first_ADRDhosp %>%
  select(c(QID, ADATE, DDATE))%>%
  rename(ADRD_hosp = ADATE, ADRD_disc = DDATE)


## Define first hospitalization (more than 30 days after first ADRD hospitalization discharge) ----

# Select patients who had a ADRD hospitalization and add the date of the first ADRD hospitalization
medpar_admissions = medpar_admissions[medpar_admissions$QID %in% first_ADRDhosp$QID,]%>%
  left_join(first_ADRDhosp)

# Adapt date format
medpar_admissions$ADATE = as.Date(medpar_admissions$ADATE, "%d%b%Y")

# Check time difference between first ADRD hospitalization discharge and other admissions 
hospitalizations = medpar_admissions %>%
  mutate(days_to_hosp_disc = as.numeric(difftime(ADATE, ADRD_disc, units =  "days")))%>%
  mutate(days_to_hosp_adm = as.numeric(difftime(ADATE, ADRD_hosp, units =  "days")))%>%
  filter(days_to_hosp_adm > 0) # consider just admissions after the first ADRD hosp


# Select first hospitalization after first ADRD discharge and select difference > 1 month
hosp_tab = data.table(hospitalizations)
first_hosp = as.data.frame(hosp_tab[hosp_tab[ , .I[which.min(ADATE)], by = QID]$V1]) %>%
  filter(days_to_hosp_disc > 30)%>% # comment if interested also in hospitalization within first month
  filter(ADM_SOURCE %in% c("7","1","2"))


first_hosp = first_hosp %>%
  rename(admission = ADATE) %>%
  mutate(DDATE = as.Date(DDATE, "%d%b%Y"))

save(first_hosp, file = "../data/scratch/hospitalization_cohort_total.RData")

# load("../data/scratch/hospitalization_cohort_total.RData")

# Warm season (May 01 - September 30)
first_hosp_warm = first_hosp %>%
  filter(month(admission) %in% 5:9)

# Select patients without other hospitalizations within the first 30 days from first hospitalization
foll_hosp = first_hosp_warm[,c("QID", "admission", "DDATE")] %>%
  rename(discharge = DDATE) %>%
  left_join(medpar_admissions[, c("QID", "ADATE", "DDATE")])%>%
  mutate(days_to_hosp_adm = as.numeric(difftime(ADATE, admission, units =  "days")))%>%
  mutate(days_to_hosp_disc = as.numeric(difftime(ADATE, discharge, units =  "days")))%>%
  subset(days_to_hosp_adm>0)

foll_hosp_tab = data.table(foll_hosp)
valid_id = as.data.frame(foll_hosp_tab[foll_hosp_tab[ , .I[which.min(ADATE)], by = QID]$V1]) %>%
  filter(days_to_hosp_disc > 30) # comment if interested also in hospitalization within first month

first_hosp_warm = first_hosp_warm[first_hosp_warm$QID %in% valid_id$QID,]

first_hosp_warm = first_hosp_warm %>%
  select(-c(days_to_hosp_disc, days_to_hosp_adm))

# save(first_hosp_warm, file = "../data/scratch/hospitalization_cohort_warmseason.RData")

### If interested in ADRD hosp only
first_hosp_warm = first_hosp_warm %>% 
  mutate(codes_num = rowSums(across(starts_with("DIAG"), ~`%in%`(.,ADRD_codes))))%>%
  filter(codes_num > 0 )

length(unique(first_hosp_warm$QID))
save(first_hosp_warm, file = "../data/scratch/hospitalization_cohort_warmseason_ADRD.RData")

## Create case-crossover data ----
# load("../data/scratch/hospitalization_cohort_warmseason.RData")

# Identify day of the week of case
extract_case_control_dates = function(case_day){
  days = this_month(case_day)$sequence[wday(this_month(case_day)$sequence) == wday(case_day)]
  return(days)
}

# Assign to every ID list of days (case + control)
ID_days = first_hosp_warm[,c("QID", "admission")] %>%
  mutate(week_days = map(admission, extract_case_control_dates))

# Create an entry per each day, add flag on hospitalization day
ID_monthdays = as.data.frame(setDT(ID_days)[, .(week_days = unlist(week_days)), by = "QID"]) %>%
  mutate(week_days = as.Date(week_days, origin = "1970-01-01")) %>%
  left_join(ID_days[,c("QID", "admission")]) %>%
  mutate(case_day = as.numeric(admission == week_days))

# Merge with patient data
patient_data = first_hosp_warm %>%
  select(c(QID, zipcode_R, AGE, Sex_gp, Race_gp, ADRD_hosp, ADRD_disc, 
           DIAG1, DIAG2, DIAG3, DIAG4, DIAG5, DIAG6, DIAG7, DIAG8, DIAG9, DIAG10))

patient_days = merge(x = patient_data, y = ID_monthdays, by = "QID")

save(patient_days, file = "../data/scratch/case-crossover-cohort_before_check.RData")
# load("../data/scratch/case-crossover-cohort.RData")
length(unique(patient_days$QID))

## Check no hospitalization on control days

# Save for each patient month-year of interest
casemonthyear_ID = patient_days %>%
  filter(case_day == 1)%>%
  mutate(month = month(admission), year = year(admission))%>%
  select(c(QID, month, year, admission))

# Select hospitalization in same month-year of case
casemonthyear_ID = casemonthyear_ID%>%
  left_join(medpar_admissions)%>%
  filter(month(ADATE)== month & year(ADATE)==year) %>%
  filter(ADATE != admission )%>%
  select(c(QID, month, year, admission, ADATE, DDATE))

any(duplicated(casemonthyear_ID))

casemonthyear_ID <- unique(casemonthyear_ID) 

# Merge with control days to check if issues and select just patient with issues
check_controldays = casemonthyear_ID %>%
  left_join(patient_days)%>%
  filter(case_day == 0 ) %>%
  mutate(control_hosp = ifelse(week_days >= ADATE & week_days <= DDATE, 1, 0))%>%
  filter(control_hosp == 1)

# save IDs of patients with at least one problematic control day
prob_IDs = unique(check_controldays$QID)
nrow(prob_IDs)

# Remove patients with num_critic > 0
final_cohort = patient_days[!patient_days$QID %in% prob_IDs,]
length(unique(final_cohort$QID))
length(unique(patient_days$QID)) - length(prob_IDs)

# Check NA
sum(is.na(final_cohort$zipcode_R)) # some NA in this variable
prob_ID = unique(final_cohort$QID[is.na(final_cohort$zipcode_R)]) # 16 problematic IDs
length(prob_ID)
final_cohort[final_cohort$QID %in%prob_ID, ]

# Remove patients with NA zipcode
final_cohort = final_cohort[!final_cohort$QID %in%prob_ID, ]
length(unique(final_cohort$QID))
save(final_cohort, file = "../data/scratch/final_cohort.RData")


# Add denom information and Koppen ----------------------------------------
Koppen_counties <- read_csv("/n/dominici_nsaph_l3/projects/temperature-adrd-casecrossover/data/Koppen_counties.csv")
load("../data/scratch/final_cohort_ADRD.RData")
final_cohort = final_cohort %>%
  mutate(year = year(admission))

# Load denom files
dir <- "/n/dominici_nsaph_l3/Lab/projects/analytic/denom_by_year/"
files <- list.files(dir, pattern = ".fst", full.names = TRUE)
cols <- c("zip", "year", "qid", "sex", "race", "age", "dual", "fips")

denom <- as.data.frame(rbindlist(lapply(files,
                                        read_fst,
                                        columns = cols,
                                        as.data.table = TRUE)))
# Merge cohort and denom data
denom_merged = merge(final_cohort, denom, 
      by.x = c("QID", "year"), by.y = c("qid", "year"))

# fip-zip x year
fips_df = denom %>%
  select(c(year, zip, fips))%>%
  group_by(year, zip)%>% 
  filter(row_number() == 1) %>%
  ungroup()%>%
  na.omit()

# merge Koppen
Koppen_counties$Koppen <- factor(Koppen_counties$Koppen, 
                                 c("BSh", "BSk", "BWh", "BWk",
                                   "Dfa", "Dfb", "Dfc", "Dsa", "Dsb", "Dwa", "Dwb",
                                   "Cfa", "Cfb", "Csa", "Csb",
                                   "Af", "Am", "Aw"),
                                 c("Semi-arid hot", "Semi-arid cold", "Desert hot", "Desert cold",
                                   "Continental hot summer", "Continental warm summer",
                                   "Continental cold summer", "Continental dry hot summer",
                                   "Continental dry warm summer", "Continental dry winter hot summer",
                                   "Continental dry winter warm summer",
                                   "Temperate hot summer", "Temperate warm summer",
                                   "Temperate dry hot summer", "Temperate dry warm summer",
                                   "Tropical rainforest", "Tropical monsoon", 
                                   "Tropical savanna (dry winter)"))

Koppen_counties$StCoFIPS = as.integer(Koppen_counties$StCoFIPS)
Koppen_zip_df <- merge(fips_df, Koppen_counties, by.x = "fips", by.y = "StCoFIPS", all.x = TRUE)

# merge data
denom_merged = merge(denom_merged, Koppen_zip_df, by = c("year", "zip"), all.x = T)
prob_ids = unique(denom_merged[is.na(denom_merged$Koppen),"QID"])


denom_merged = denom_merged[!denom_merged$QID %in% prob_ids,]

denom_merged$Koppen_zone <- factor(denom_merged$Koppen,
                                  c("Semi-arid hot", "Semi-arid cold", "Desert hot", "Desert cold",
                                    "Continental hot summer", "Continental warm summer",
                                    "Continental cold summer", "Continental dry hot summer",
                                    "Continental dry warm summer", "Continental dry winter hot summer",
                                    "Continental dry winter warm summer",
                                    "Temperate hot summer", "Temperate warm summer",
                                    "Temperate dry hot summer", "Temperate dry warm summer",
                                    "Tropical rainforest", "Tropical monsoon", 
                                    "Tropical savanna (dry winter)"), 
                                  c("B", "B", "B", "B",
                                    "D", "D", "D", "D", "D", "D", "D",
                                    "C", "C", "C", "C",
                                    "A", "A", "A"))
table(denom_merged$Koppen_zone)
length(unique(denom_merged$QID))

save(denom_merged, file = "../data/scratch/final_cohort.RData")
