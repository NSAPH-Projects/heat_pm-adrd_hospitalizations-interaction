library(data.table)
library(dplyr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggplot2)
library(viridis)
setwd("/n/dominici_nsaph_l3/Lab/projects/heat_pm-adrd_hospitalizations-interaction/code")

load("../data/input/rehospitalization_data.RData")

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
first_rehosp %>%
  ggplot( aes(x=days_to_rehosp)) +
  geom_histogram( fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Distribution of 'days to first all-cause rehospitalization': from 31 days to 16 years") +
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

first_rehosp %>%
  ggplot( aes( x=days_to_rehosp)) +
  geom_boxplot(color="#69b3a2") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  ggtitle("Distribution of 'days to first all-cause rehospitalization': from 31 days to 16 years") +
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

# Removing hospitalizations after 5 years
first_rehosp[first_rehosp$days_to_rehosp<= 5*365,] %>%
  ggplot( aes(x=days_to_rehosp)) +
  geom_histogram( fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Distribution of 'days to first all-cause rehospitalization': from 31 days to 5 years") +
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

first_rehosp[first_rehosp$days_to_rehosp<= 5*365,] %>%
  ggplot( aes( x=days_to_rehosp)) +
  geom_boxplot(color="#69b3a2") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  ggtitle("Distribution of 'days to first all-cause rehospitalization': from 31 days to 5 years") +
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

## Percentage 

# Rehospitalizations within 6 months
nrow(first_rehosp[first_rehosp$days_to_rehosp <= 6*30,])/N*100 # 45.78%

# Rehospitalizations within 1 year
nrow(first_rehosp[first_rehosp$days_to_rehosp <= 365,])/N*100 # 65.48%

# Rehospitalizations 2+ years
nrow(first_rehosp[first_rehosp$days_to_rehosp >= 2*365,])/N*100 # 16.66%

# Rehospitalizations 5+ years
nrow(first_rehosp[first_rehosp$days_to_rehosp >= 5*365,])/N*100 # 2.44%
