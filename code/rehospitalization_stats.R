library(data.table)
library(dplyr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggpubr)
setwd("/n/dominici_nsaph_l3/Lab/projects/heat_pm-adrd_hospitalizations-interaction/code")

# load("../data/input/rehospitalization_data_total.RData")
load("../data/input/rehospitalization_data_warmseason.RData")

first_rehosp = first_rehosp_warm

N = nrow(first_rehosp)
any(duplicated(first_rehosp$QID))

first_rehosp[first_rehosp$days_to_rehosp > 5*365,]
first_rehosp[which.max(first_rehosp$days_to_rehosp),]

hist(first_rehosp$AGE)
min(first_rehosp$AGE)
max(first_rehosp$AGE)
length(first_rehosp[first_rehosp$AGE >=110, "AGE"])

## Distribution: days to rehospitalization ----
# Note: we are considering the first all-cause hospitalization occuring after 1 month from the first ADRD hospitalization

# Complete dataset
p1 <- first_rehosp %>%
  ggplot( aes(x=days_to_rehosp)) +
  geom_histogram( fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  # ggtitle("Distribution of 'days to first all-cause rehospitalization': from 31 days to 16 years") +
  theme(
    plot.title = element_text(size=15)
  )+ 
  geom_vline(xintercept = 365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 2*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 3*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 4*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 5*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 6*365, linetype="dotted", 
             color = "red", size=.5)+
  xlab("Days to first all-cause rehospitalization")

p2 <- first_rehosp %>%
  ggplot( aes( x=days_to_rehosp)) +
  geom_boxplot(color="#69b3a2") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  # ggtitle("Distribution of 'days to first all-cause rehospitalization': from 31 days to 16 years") +
  xlab("Days to first all-cause rehospitalization")+
  geom_vline(xintercept = 365, linetype="dotted", 
                       color = "red", size=.75)+ 
  geom_vline(xintercept = 2*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 3*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 4*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 5*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 6*365, linetype="dotted", 
             color = "red", size=.5)


plot1<- ggarrange(p1, p2, ncol=1, nrow=2)

annotate_figure(plot1, top = text_grob("Distribution of 'days to first all-cause rehospitalization': from 31 days to 17 years", 
                                      color = "black", face = "bold", size = 15))



# Removing hospitalizations after 5 years
p3 <- first_rehosp[first_rehosp$days_to_rehosp<= 5*365,] %>%
  ggplot( aes(x=days_to_rehosp)) +
  geom_histogram( fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  #ggtitle("Distribution of 'days to first all-cause rehospitalization': from 31 days to 5 years") +
  theme(
    plot.title = element_text(size=15)
  )+ 
  geom_vline(xintercept = 365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 2*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 3*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 4*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 5*365, linetype="dotted", 
             color = "red", size=.5)+
  xlab("Days to first all-cause rehospitalization")

p4 <- first_rehosp[first_rehosp$days_to_rehosp<= 5*365,] %>%
  ggplot( aes( x=days_to_rehosp)) +
  geom_boxplot(color="#69b3a2") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  # ggtitle("Distribution of 'days to first all-cause rehospitalization': from 31 days to 5 years") +
  xlab("Days to first all-cause rehospitalization")+ 
  geom_vline(xintercept = 365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 2*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 3*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 4*365, linetype="dotted", 
             color = "red", size=.5)+ 
  geom_vline(xintercept = 5*365, linetype="dotted", 
             color = "red", size=.5)

plot2<- ggarrange(p3, p4, ncol=1, nrow=2)

annotate_figure(plot2, top = text_grob("Distribution of 'days to first all-cause rehospitalization': from 31 days to 5 years", 
                                       color = "black", face = "bold", size = 15))

## Percentage 
N

# Rehospitalizations within 6 months
nrow(first_rehosp[first_rehosp$days_to_rehosp <= 6*30,])/N*100

# Rehospitalizations within 1 year
nrow(first_rehosp[first_rehosp$days_to_rehosp <= 365,])/N*100 

# Rehospitalizations 2+ years
nrow(first_rehosp[first_rehosp$days_to_rehosp >= 2*365,])/N*100 

# Rehospitalizations 5+ years
nrow(first_rehosp[first_rehosp$days_to_rehosp >= 5*365,])/N*100 


## How many rehosp are ADRD hosp? ----

# Total
# select first rehospitalizations that are ADRD hospitalizations
ADRD_rehosp = first_rehosp[,c('QID', 'readmission', 'DDATE', 'ADRD_hosp')] %>%
  semi_join(ADRD_hospitalization, by = c('QID', 'readmission' = 'ADATE', 'DDATE'))

any(duplicated(ADRD_rehosp))
any(duplicated(ADRD_rehosp$QID))

# Percentage
nrow(ADRD_rehosp) / nrow(first_rehosp) *100

# Check
ADRD_rehosp[1,]
medpar_admissions[medpar_admissions$QID ==ADRD_rehosp$QID[1],]
ADRD_hospitalization[ADRD_hospitalization$QID ==ADRD_rehosp$QID[1],]

nonADRD_rehosp_ID = first_rehosp$QID [!(first_rehosp$QID %in% ADRD_rehosp$QID)]
first_rehosp[first_rehosp$QID == nonADRD_rehosp_ID[1],]
medpar_admissions[medpar_admissions$QID == nonADRD_rehosp_ID[1],]
ADRD_hospitalization[ADRD_hospitalization$QID == nonADRD_rehosp_ID[1],]


# Warm season
# select first rehospitalizations that are ADRD hospitalizations
ADRD_rehosp_warm = first_rehosp_warm[,c('QID', 'readmission', 'DDATE', 'ADRD_hosp')] %>%
  semi_join(ADRD_hospitalization, by = c('QID', 'readmission' = 'ADATE', 'DDATE'))

any(duplicated(ADRD_rehosp_warm))
any(duplicated(ADRD_rehosp_warm$QID))

# Percentage
nrow(ADRD_rehosp_warm) / nrow(first_rehosp_warm) *100

# Check
ADRD_rehosp_warm[1,]
medpar_admissions[medpar_admissions$QID ==ADRD_rehosp_warm$QID[1],]
ADRD_hospitalization[ADRD_hospitalization$QID ==ADRD_rehosp_warm$QID[1],]

nonADRD_rehosp_ID = first_rehosp_warm$QID [!(first_rehosp_warm$QID %in% ADRD_rehosp_warm$QID)]

first_rehosp_warm[first_rehosp_warm$QID == nonADRD_rehosp_ID[1],]
medpar_admissions[medpar_admissions$QID == nonADRD_rehosp_ID[1],]
ADRD_hospitalization[ADRD_hospitalization$QID == nonADRD_rehosp_ID[1],]


