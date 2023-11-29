# Required libraries
library(data.table)
library(lubridate)
library(weathermetrics)
library(dplyr)
library(fst)
library(survival)
library(ggplot2)
setwd("~/heat_pm-adrd_hospitalizations-interaction/code")

load("../data/scratch/case-crossover-exposure.Rdata")


# PM2.5
pm_lag0_raw <- clogit(case_day ~ pm25 + strata(QID), 
                        data = data)
pm_lag0 = summary(pm_lag0_raw)

exp(IQR(data$pm25) * pm_lag0$coefficients[1])
c(exp(IQR(data$pm25) * pm_lag0$coefficients[1] - 1.96 * pm_lag0$coefficients[3]), exp(IQR(data$pm25) * pm_lag0$coefficients[1] + 1.96 * pm_lag0$coefficients[3]))

# Heat index
heat_lag0_raw <- clogit(case_day ~ heat_index + strata(QID), 
                          data = data)
heat_lag0 = summary(heat_lag0_raw)

exp(heat_lag0$coefficients[1])
c(exp(heat_lag0$coefficients[1] - 1.96 * heat_lag0$coefficients[3]), exp( heat_lag0$coefficients[1] + 1.96 * heat_lag0$coefficients[3]))


# All exposure
exp_lag0_raw <- clogit(case_day ~ pm25 + heat_index + no2 + ozone + strata(QID), 
                   data = data)
exp_lag0 = summary(exp_lag0_raw)

exp(IQR(data$pm25) * exp_lag0$coefficients[1])
c(exp(IQR(data$pm25) * exp_lag0$coefficients[1] - 1.96 * exp_lag0$coefficients[1,3]), exp(IQR(data$pm25) * exp_lag0$coefficients[1] + 1.96 * exp_lag0$coefficients[1,3]))

exp(exp_lag0$coefficients[2])
c(exp( exp_lag0$coefficients[2] - 1.96 * exp_lag0$coefficients[2,3]), exp(exp_lag0$coefficients[2] + 1.96 * exp_lag0$coefficients[2,3]))

exp(IQR(data$no2) * exp_lag0$coefficients[3])
c(exp(IQR(data$no2) * exp_lag0$coefficients[3] - 1.96 * exp_lag0$coefficients[3,3]), exp(IQR(data$no2) * exp_lag0$coefficients[3] + 1.96 * exp_lag0$coefficients[3,3]))

exp(IQR(data$ozone) * exp_lag0$coefficients[4])
c(exp(IQR(data$ozone) * exp_lag0$coefficients[4] - 1.96 * exp_lag0$coefficients[4,3]), exp(IQR(data$ozone) * exp_lag0$coefficients[4] + 1.96 * exp_lag0$coefficients[4,3]))


# All exposure + Interaction 
interaction_lag0_raw <- clogit(case_day ~ pm25 + heat_index + no2 + ozone + pm25*heat_index + strata(QID), 
                   data = data)
interaction_lag0 = summary(interaction_lag0_raw)


exp(IQR(data$pm25) * interaction_lag0$coefficients[1])
c(exp(IQR(data$pm25) * interaction_lag0$coefficients[1] - 1.96 * interaction_lag0$coefficients[1,3]), exp(IQR(data$pm25) * interaction_lag0$coefficients[1] + 1.96 * interaction_lag0$coefficients[1,3]))

exp(interaction_lag0$coefficients[2])
c(exp( interaction_lag0$coefficients[2] - 1.96 * interaction_lag0$coefficients[2,3]), exp(interaction_lag0$coefficients[2] + 1.96 * interaction_lag0$coefficients[2,3]))

exp(IQR(data$no2) * interaction_lag0$coefficients[3])
c(exp(IQR(data$no2) * interaction_lag0$coefficients[3] - 1.96 * interaction_lag0$coefficients[3,3]), exp(IQR(data$no2) * interaction_lag0$coefficients[3] + 1.96 * interaction_lag0$coefficients[3,3]))

exp(IQR(data$ozone) * interaction_lag0$coefficients[4])
c(exp(IQR(data$ozone) * interaction_lag0$coefficients[4] - 1.96 * interaction_lag0$coefficients[4,3]), exp(IQR(data$ozone) * interaction_lag0$coefficients[4] + 1.96 * interaction_lag0$coefficients[4,3]))


exp(IQR(data$pm25) * interaction_lag0$coefficients[5])
c(exp(IQR(data$pm25) * interaction_lag0$coefficients[5] - 1.96 * interaction_lag0$coefficients[5,3]), exp(IQR(data$pm25) * interaction_lag0$coefficients[5] + 1.96 * interaction_lag0$coefficients[5,3]))



# Interaction heat map

grids <- expand.grid(PM = seq(quantile(data$pm25, 0.01), quantile(data$pm25, 0.99), by = 0.1), HI = seq(quantile(data$heat_index, 0.01), quantile(data$heat_index, 0.99), by = 0.1))
grids$PM_med <- grids$PM - median(data$pm25) 
grids$HI_med  <- grids$HI - median(data$heat_index) 
grids$Value <- exp(grids$PM_med*interaction_lag0$coefficients[1] + grids$HI_med*interaction_lag0$coefficients[2] + grids$PM_med * grids$HI_med * interaction_lag0$coefficients[5])

# Create a heatmap using ggplot2
ggplot(grids, aes(x = PM_med, y = HI_med, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "exp{(PM2.5 - Median(PM2.5))*B_pm + (HI - Median(HI))*B_heat + (PM2.5 - Median(PM2.5))*(HI - Median(HI))*B_int}",
       x = "PM2.5 - Median(PM2.5)",
       y = "Heat Index - Median(Heat Index)",
       fill = "Value") + 
      geom_vline(xintercept = 0, linetype="dotted", 
             color = "blue", size=.5)+ 
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "blue", size=.5)+
  theme_minimal()



# All exposure + Interaction (NO OZONE)
interaction_noozone_lag0_raw <- clogit(case_day ~ pm25 + heat_index + no2  + pm25*heat_index + strata(QID), 
                               data = data)
interaction_noozone_lag0 = summary(interaction_noozone_lag0_raw)


exp(IQR(data$pm25) * interaction_noozone_lag0$coefficients[1])
c(exp(IQR(data$pm25) * interaction_noozone_lag0$coefficients[1] - 1.96 * interaction_noozone_lag0$coefficients[1,3]), exp(IQR(data$pm25) * interaction_noozone_lag0$coefficients[1] + 1.96 * interaction_noozone_lag0$coefficients[1,3]))

exp(interaction_noozone_lag0$coefficients[2])
c(exp( interaction_noozone_lag0$coefficients[2] - 1.96 * interaction_noozone_lag0$coefficients[2,3]), exp(interaction_noozone_lag0$coefficients[2] + 1.96 * interaction_noozone_lag0$coefficients[2,3]))

exp(IQR(data$no2) * interaction_noozone_lag0$coefficients[3])
c(exp(IQR(data$no2) * interaction_noozone_lag0$coefficients[3] - 1.96 * interaction_noozone_lag0$coefficients[3,3]), exp(IQR(data$no2) * interaction_noozone_lag0$coefficients[3] + 1.96 * interaction_noozone_lag0$coefficients[3,3]))


exp(IQR(data$pm25) * interaction_noozone_lag0$coefficients[4])
c(exp(IQR(data$pm25) * interaction_noozone_lag0$coefficients[4] - 1.96 * interaction_noozone_lag0$coefficients[4,3]), exp(IQR(data$pm25) * interaction_noozone_lag0$coefficients[4] + 1.96 * interaction_noozone_lag0$coefficients[4,3]))



# Interaction heat map

grids <- expand.grid(PM = seq(quantile(data$pm25, 0.01), quantile(data$pm25, 0.99), by = 0.1), HI = seq(quantile(data$heat_index, 0.01), quantile(data$heat_index, 0.99), by = 0.1))
grids$PM_med <- grids$PM - median(data$pm25) 
grids$HI_med  <- grids$HI - median(data$heat_index) 
grids$Value <- exp(grids$PM_med*interaction_noozone_lag0$coefficients[1] + grids$HI_med*interaction_noozone_lag0$coefficients[2] + grids$PM_med * grids$HI_med * interaction_noozone_lag0$coefficients[4])

# Create a heatmap using ggplot2
ggplot(grids, aes(x = PM_med, y = HI_med, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "exp{(PM2.5 - Median(PM2.5))*B_pm + (HI - Median(HI))*B_heat + (PM2.5 - Median(PM2.5))*(HI - Median(HI))*B_int}",
       x = "PM2.5 - Median(PM2.5)",
       y = "Heat Index - Median(Heat Index)",
       fill = "Value") + 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "blue", size=.5)+ 
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "blue", size=.5)+
  theme_minimal()
