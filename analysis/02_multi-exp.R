# Required libraries
library(data.table)
library(lubridate)
library(weathermetrics)
library(dplyr)
library(fst)
library(viridis)
library(plotly)
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

# No interaction ------------------------------------------------------------

### Linear
lag0_raw <- clogit(case_day ~ pm_lag_0 + heat_index_lag_0 + strata(QID), 
                         data = data)
exp_lag0 = summary(lag0_raw)

# PM
round(exp(5 * exp_lag0$coefficients[1]),4)
round(c(exp(5 * (exp_lag0$coefficients[1] - 1.96 * exp_lag0$coefficients[1,3])), exp(5 * (exp_lag0$coefficients[1] + 1.96 * exp_lag0$coefficients[1,3]))),4)

# HI
round(exp(0.49*exp_lag0$coefficients[2]),4)
round(c(exp( 0.49*(exp_lag0$coefficients[2] - 1.96 * exp_lag0$coefficients[2,3])), exp(0.49*(exp_lag0$coefficients[2] + 1.96 * exp_lag0$coefficients[2,3]))),4)


### Non linear
cb_heat <- crossbasis(data[,paste0("heat_index_lag_",0)],
                      lag = 0, 
                      argvar = list(fun = "bs", df = 4))

cb_pm <- crossbasis(data[,paste0("pm_lag_",0)],
                    lag = 0, 
                    argvar = list(fun = "bs", df = 4))

model <- clogit(case_day ~ cb_heat + cb_pm + strata(QID), 
                data)

AIC(model)
cp_heat <- crosspred(cb_heat, model, at = 1:999/1000, cen = 0.5, cumul = TRUE)
plot(cp_heat, main = expression(bold("Joint exposure - HI: Odds ratio wrt 50% percentile")), xlab = "Heat index percentile")

cp_heat$allRRfit["0.99"]
cbind(cp_heat$allRRlow, cp_heat$allRRhigh)["0.99",]

cp_pm <- crosspred(cb_pm, model, at=seq(0, 36, by=0.1),cen = 5,  cumul = TRUE)
plot(cp_pm, main = expression(bold("Joint exposure - PM"[2.5]) ~ bold(": Odds ratio wrt 5") ~ bold(mu*"g/m"^3)), xlab = expression("PM"[2.5]) )

round(cp_pm$allRRfit["10"],3)
round(cbind(cp_pm$allRRlow, cp_pm$allRRhigh)["10",],3)

cp_pm <- crosspred(cb_pm, model, at=seq(0, 36, by=0.1),cen = 10,  cumul = TRUE)

round(cp_pm$allRRfit["15"],3)
round(cbind(cp_pm$allRRlow, cp_pm$allRRhigh)["15",],3)

# Linear interaction ------------------------------------------------------------

lag0_inter_raw <- clogit(case_day ~ pm_lag_0 + heat_index_lag_0 + pm_lag_0*heat_index_lag_0 + 
                           strata(QID), 
                         data = data)
exp_lag0 = summary(lag0_inter_raw)


# PM
round(exp(5 * exp_lag0$coefficients[1]),4)
round(c(exp(5 * (exp_lag0$coefficients[1] - 1.96 * exp_lag0$coefficients[1,3])), exp(5 * (exp_lag0$coefficients[1] + 1.96 * exp_lag0$coefficients[1,3]))),4)

# HI
round(exp(0.49*exp_lag0$coefficients[2]),4)
round(c(exp( 0.49*(exp_lag0$coefficients[2] - 1.96 * exp_lag0$coefficients[2,3])), exp(0.49*(exp_lag0$coefficients[2] + 1.96 * exp_lag0$coefficients[2,3]))),4)

# Interaction term
round(exp(5 * 0.49 * exp_lag0$coefficients[3]),4)
round(c(exp(5 * 0.49 * (exp_lag0$coefficients[3] - 1.96 * exp_lag0$coefficients[3,3])), exp(5 * 0.49 * (exp_lag0$coefficients[3] + 1.96 * exp_lag0$coefficients[3,3]))),4)

# Interaction heat map
grids <- expand.grid(PM = seq(quantile(data$pm_lag_0, 0.01), quantile(data$pm_lag_0, 0.99), length.out = 200), HI = seq(quantile(data$heat_index_lag_0, 0.01), quantile(data$heat_index_lag_0, 0.99), length.out = 200))
grids$PM_med <- grids$PM - 5
grids$HI_med  <- grids$HI - median(data$heat_index_lag_0) 
grids$Value <- exp(grids$PM_med*exp_lag0$coefficients[1] + grids$HI_med*exp_lag0$coefficients[2] + grids$PM_med * grids$HI_med * exp_lag0$coefficients[3])

ggplot(grids, aes(x = HI, y = PM, z = Value)) +
  geom_contour_filled() +
  scale_fill_viridis_d() +
  labs(title = "Odds Ratio Plot wrt reference level (0.5, 5)",
       x = "Heat Index", y = expression("PM"[2.5]), fill = "Odds Ratio")+
  theme_minimal()

z_flat <- matrix(1, nrow = length(unique(grids$HI)), ncol = length(unique(grids$PM)))

p <- plot_ly(grids, x = ~unique(HI), y = ~unique(PM), z = ~matrix(Value, nrow=length(unique(grids$PM))), type = "surface") %>%
  add_surface(x = ~unique(HI), y = ~unique(PM), z = ~z_flat, showscale = FALSE, opacity = 0.5)%>%
  layout(
    scene = list(
      xaxis = list(title = "Heat index"),
      yaxis = list(title = "PM<sub>2.5</sub>"), 
      zaxis = list(title = "Odds ratio")
    ),
    title = list(
      text = "Odds ratio compared to HI:0.5 and PM<sub>2.5</sub>:5",  
      font = list(size = 15)  
    ),
    margin = list(t = 100)  
  ) %>%
  colorbar(title = "OR")

p
