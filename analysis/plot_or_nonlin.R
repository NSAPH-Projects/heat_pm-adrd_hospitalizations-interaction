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
  # mutate_at(vars(pm_lag_0, pm_lag_1, pm_lag_2), ~ ifelse(. > pm_99perc, pm_99perc, .))%>% #for capping
  mutate(hi3avg = (heat_index_lag_0 + heat_index_lag_1 + heat_index_lag_2 )/3)%>%
  mutate(pm3avg = (pm_lag_0 + pm_lag_1 + pm_lag_2 )/3) 

# Single exposure models --------------------------------------------------
## Heat index
cb_single_hi <- crossbasis(data[,paste0("heat_index_lag_",0)],
                 lag = 0, 
                 argvar = list(fun = "bs", df = 4))
model_single_hi <- clogit(case_day ~ cb_single_hi + strata(QID), 
                data)

cp_single_hi <- crosspred(cb_single_hi, model_single_hi, at = 1:999/1000, cen = 0.5, cumul = TRUE)
plot(cp_single_hi, main = paste0("Heat index - Odds ratio for changes from median"), xlab = "Heat index percentile", ylab = "OR" ) # PLOT 1

## PM2.5
cb_single_pm <- crossbasis(data[,paste0("pm_lag_",0)],
                 lag = 0, 
                 argvar = list(fun = "bs", df = 4))
model_single_pm <- clogit(case_day ~ cb_single_pm + strata(QID), 
                data)

cp_single_pm <- crosspred(cb_single_pm, model_single_pm, at=seq(0, 36, by=0.1),cen = 5,  cumul = TRUE)
plot(cp_single_pm, main = expression(bold("PM"[2.5]) ~ bold("- Odds ratio for changes from 5") ~ bold(mu*"g/m"^3)), 
     xlab = expression("PM"[2.5]), ylab = "OR" ) # PLOT 2 


# Joint-exposure model --------------------------------------------------

cb_heat <- crossbasis(data[,paste0("heat_index_lag_",0)],
                      lag = 0, 
                      argvar = list(fun = "bs", df = 4))

cb_pm <- crossbasis(data[,paste0("pm_lag_",0)],
                    lag = 0, 
                    argvar = list(fun = "bs", df = 4))

model <- clogit(case_day ~ cb_heat + cb_pm + strata(QID), 
                data)

cp_heat <- crosspred(cb_heat, model, at = 1:999/1000, cen = 0.5, cumul = TRUE)
plot(cp_heat, main = paste0("Heat index- Odds ratio for changes from median"), xlab = "Heat index percentile", ylab = "OR") # PLOT 3

cp_pm <- crosspred(cb_pm, model, at=seq(0, 36, by=0.1),cen = 5,  cumul = TRUE)
plot(cp_pm, main = expression(bold("PM"[2.5]) ~ bold("- Odds ratio for changes from 5") ~ bold(mu*"g/m"^3)), 
     xlab = expression("PM"[2.5]*" ("*mu*"g/m"^3*")"), ylab = "OR") # PLOT 4

## Plot 
png("Figure_2.png", width = 14, height = 8, units = "in", res = 300)

# Layout matrix: 3 rows (2 for plots, 1 blank row in the middle)
# 0 means blank cell
layout_matrix <- matrix(c(1, 2,
                          0, 0,
                          3, 4), 
                        nrow = 3, byrow = TRUE)

# Use layout with heights: 1 for each plot row, 0.3 for the blank spacer row
layout(layout_matrix, heights = c(1, 0.3, 1))

# Set outer and inner margins
par(oma = c(1, 4, 2, 1), mar = c(4, 4, 2, 1), cex.main = 1.6,    # Title size
    cex.lab = 1.5,     # Axis label size
    cex.axis = 1.4)

# Plot 1
plot(cp_single_hi,
     main = "Heat index - Odds ratio for changes from median",
     xlab = "Heat index percentile",
     ylab = "OR")

# Plot 2
plot(cp_single_pm,
     main = expression(bold("PM"[2.5]) ~ bold("- Odds ratio for changes from 5") ~ bold(mu*"g/m"^3)),
     xlab = expression("PM"[2.5]*" ("*mu*"g/m"^3*")"),
     ylab = "OR")


# Plot 3
plot(cp_heat,
     main = "Heat index - Odds ratio for changes from median",
     xlab = "Heat index percentile",
     ylab = "OR")

# Plot 4
plot(cp_pm,
     main = expression(bold("PM"[2.5]) ~ bold("- Odds ratio for changes from 5") ~ bold(mu*"g/m"^3)),
     xlab = expression("PM"[2.5]*" ("*mu*"g/m"^3*")"),
     ylab = "OR")

# Add section labels
mtext("A) Single exposure models", side = 3, line = 0.5, outer = TRUE, adj = 0, font = 2)
mtext("B) Joint-exposure model",  side = 3, line = -30, outer = TRUE, adj = 0, font = 2)

# Close device
dev.off()
