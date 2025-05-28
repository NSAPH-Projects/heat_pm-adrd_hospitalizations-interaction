# Required libraries
library(data.table)
library(lubridate)
library(weathermetrics)
library(dplyr)
library(fst)
library(splines)
library(viridis)
library(ggpubr)
library(survival)
library(ggplot2)
library(dlnm)
setwd("/n/dominici_nsaph_l3/Lab/projects/heat_pm-adrd_hospitalizations-interaction/code")

# Load data
load("../data/scratch/cohort_lagged_dat.Rdata")
pm_99perc = quantile(data$pm_lag_0, 0.99)
data = data %>%
  # mutate_at(vars(pm_lag_0, pm_lag_1, pm_lag_2), ~ ifelse(. > pm_99perc, pm_99perc, .))%>%
  mutate(hi3avg = (heat_index_lag_0 + heat_index_lag_1 + heat_index_lag_2 )/3)%>%
  mutate(pm3avg = (pm_lag_0 + pm_lag_1 + pm_lag_2 )/3) 

# Single exposure models --------------------------------------------------

## Heat index
cb <- crossbasis(data[,paste0("heat_index_lag_",0)],
                 lag = 0, 
                 argvar = list(fun = "bs", df = 4))
model <- clogit(case_day ~ cb + strata(QID), 
                data)

cp <- crosspred(cb, model, at = 1:999/1000, cen = 0.5, cumul = TRUE)

plot(cp, main = expression(bold("Single exposure - HI: Odds ratio wrt 50% percentile")), xlab = "Heat index percentile" )

round(cp$allRRfit["0.99"],3)
round(cbind(cp$allRRlow, cp$allRRhigh)["0.99",],3)

## linear regression on heat index
mod <- clogit(case_day ~ heat_index_lag_0 + strata(QID), data = data)

cat("print OR for 0.49 increase in heat index percentile and its 95CI \n")
est <- coef(mod)[['heat_index_lag_0']]; se <- sqrt(vcov(mod)[1,1])
round(exp(est*0.49),3)
round(exp((est-qnorm(.975)*se)*0.49),3)
round(exp((est+qnorm(.975)*se)*0.49),3)


## PM2.5
cb <- crossbasis(data[,paste0("pm_lag_",0)],
                 lag = 0, 
                 argvar = list(fun = "bs", df = 4))
model <- clogit(case_day ~ cb + strata(QID), 
                data)
AIC(model)

cp <- crosspred(cb, model, at=seq(0, 36, by=0.1),cen = 5,  cumul = TRUE)

plot(cp, main = expression(bold("Single exposure - PM"[2.5]) ~ bold(": Odds ratio wrt 5") ~ bold(mu*"g/m"^3)), xlab = expression("PM"[2.5]) )

round(cp$allRRfit["10"],3)
round(cbind(cp$allRRlow, cp$allRRhigh)["10",],3)

cp <- crosspred(cb, model, at=seq(0, 36, by=0.1),cen = 10,  cumul = TRUE)

round(cp$allRRfit["15"],3)
round(cbind(cp$allRRlow, cp$allRRhigh)["15",],3)

## linear regression on heat index
mod <- clogit(case_day ~ pm_lag_0 + strata(QID), data = data)
print(summary(mod))
est <- coef(mod)[['pm_lag_0']]; se <- sqrt(vcov(mod)[1,1])

cat("print OR for 5 units increase in PM2.5 and its 95CI \n")
round(exp(est*5),3)
round(exp((est-qnorm(.975)*se)*5),3)
round(exp((est+qnorm(.975)*se)*5),3)
