library(fst)
library(data.table)
library(dplyr)
library(tidyverse)
library(magrittr)
library(lubridate)
setwd("/n/dominici_nsaph_l3/Lab/projects/heat_pm-adrd_hospitalizations-interaction/code")

## Get data ----
# Get ADRD hospitalizations data
f <- list.files("/n/dominici_nsaph_l3/Lab/projects/analytic/adrd_hospitalization",
                pattern = "\\.fst",
                full.names = TRUE)

ADRD_hospitalization <- as.data.frame(rbindlist(lapply(f,
                                                       read_fst,
                                                       as.data.table = TRUE))) %>% 
  filter(AGE < 100)%>%
  mutate(QID = as.character(QID))

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
  mutate(QID = as.character(QID))

## Define first ADRD hospitalization ----
hosp_tab = data.table(ADRD_hospitalization)
first_ADRDhosp = as.data.frame(hosp_tab[hosp_tab[ , .I[which.min(ADATE)], by = QID]$V1])
head(first_ADRDhosp)

length(unique(ADRD_hospitalization$QID))
dim(first_ADRDhosp)

# Save first ADRD hospitalization for every patient (later may need to save and select patients based on billing codes)
first_ADRDhosp = first_ADRDhosp %>%
  select(c(QID, ADATE))%>%
  rename(ADRD_hosp = ADATE)


## Define first rehospitalization (more than 30 days after first ADRD hospitalization) ----

# Select patients who had a ADRD hospitalization and add the date of the first ADRD hospitalization
medpar_admissions = medpar_admissions[medpar_admissions$QID %in% first_ADRDhosp$QID,]%>%
   left_join(first_ADRDhosp)

length(unique(medpar_admissions$QID))
sum(is.na(medpar_admissions$ADRD_hosp))

# Adapt date format
medpar_admissions$ADATE = as.Date(medpar_admissions$ADATE, "%d%b%Y")
head(medpar_admissions)

# Check time difference between first ADRD hospitalization and other admissions 
rehospitalizations = medpar_admissions %>%
  mutate(days_to_rehosp = as.numeric(difftime(ADATE, ADRD_hosp, units =  "days")))%>%
  subset(days_to_rehosp > 0)

summary(rehospitalizations$days_to_rehosp)

# Select first rehospitalization after first ADRD admission and select difference > 1 month
rehosp_tab = data.table(rehospitalizations)
first_rehosp = as.data.frame(rehosp_tab[rehosp_tab[ , .I[which.min(ADATE)], by = QID]$V1])%>%
  filter(days_to_rehosp > 30)

head(first_rehosp)
summary(first_rehosp$days_to_rehosp)

first_rehosp = first_rehosp %>%
  rename(readmission = ADATE) %>%
  mutate(DDATE = as.Date(DDATE, "%d%b%Y"))

save(first_rehosp, file = "../data/input/rehospitalization_data_total.RData")

load("../data/input/rehospitalization_data_total.RData")

# Warm season (May 01 - September 30)
first_rehosp_warm = first_rehosp %>%
  filter(month(readmission) %in% 5:9)

save(first_rehosp_warm, file = "../data/input/rehospitalization_data_warmseason.RData")




## Check ----

# Check age people who had a late rehospitalization

hist(first_rehosp$AGE[first_rehosp$days_to_rehosp > 5*365])



# Check people who didn't get a rehospitalization 

no_rehosp = first_ADRDhosp$QID[!(first_ADRDhosp$QID %in% first_rehosp$QID)]
length(no_rehosp)/length(first_ADRDhosp$QID) # 0.3825927

rehosp_onlywithin30d = medpar_admissions[medpar_admissions$QID %in% no_rehosp,] %>%
  mutate(days_to_rehosp = as.numeric(difftime(ADATE, ADRD_hosp, units =  "days")))%>%
  subset(days_to_rehosp > 0 & days_to_rehosp <=30)

length(unique(rehosp_onlywithin30d$QID)) # number of patients that only had rehospitalizations within 30 days from first ADRD 

length(unique(rehosp_onlywithin30d$QID))/length(first_ADRDhosp$QID)
# 0.06132273

length(unique(rehosp_onlywithin30d$QID))/length(no_rehosp)
# 0.160282

hist(first_rehosp$days_to_rehosp)
boxplot(first_rehosp$days_to_rehosp)
