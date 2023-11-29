# Required libraries
library(data.table)
library(lubridate)
library(weathermetrics)
library(dplyr)
library(fst)
library(viridis)
library(ggpubr)
library(survival)
library(ggplot2)
library(dlnm)
setwd("~/heat_pm-adrd_hospitalizations-interaction/code")

# Load data
load("../data/scratch/cohort_lagged_dat.Rdata")

# Exposure variables
heat_var <- c("heat_index_lag_0", "heat_index_lag_1", "heat_index_lag_2", 
             "heat_index_lag_3", "heat_index_lag_4", "heat_index_lag_5", 
             "heat_index_lag_6", "heat_index_lag_7", "heat_index_lag_8", 
             "heat_index_lag_9", "heat_index_lag_10", "heat_index_lag_11", 
             "heat_index_lag_12", "heat_index_lag_13", "heat_index_lag_14")
no2_var <- c("no2_lag_0", "no2_lag_1", "no2_lag_2", "no2_lag_3", "no2_lag_4", 
             "no2_lag_5", "no2_lag_6", "no2_lag_7", "no2_lag_8", "no2_lag_9", 
             "no2_lag_10", "no2_lag_11", "no2_lag_12", "no2_lag_13",
             "no2_lag_14")
ozone_var <- c("ozone_lag_0", "ozone_lag_1", "ozone_lag_2", "ozone_lag_3", 
               "ozone_lag_4", "ozone_lag_5", "ozone_lag_6", "ozone_lag_7", 
               "ozone_lag_8", "ozone_lag_9", "ozone_lag_10", "ozone_lag_11", 
               "ozone_lag_12", "ozone_lag_13", "ozone_lag_14")
pm_var <- c("pm_lag_0", "pm_lag_1", "pm_lag_2", "pm_lag_3", "pm_lag_4", 
            "pm_lag_5", "pm_lag_6", "pm_lag_7", "pm_lag_8", "pm_lag_9", 
            "pm_lag_10", "pm_lag_11", "pm_lag_12", "pm_lag_13",
            "pm_lag_14") 
 
# Exposure correlation ----------------------------------------------------

# PM2.5 - Ozone
cor_pm_oz <- cor(x = as.data.frame(data)[,pm_var], y =as.data.frame(data)[,ozone_var])
corrplot::corrplot(cor_pm_oz)

# PM2.5 - NO2
cor_pm_no2 <- cor(x = as.data.frame(data)[,pm_var], y =as.data.frame(data)[,no2_var])
corrplot::corrplot(cor_pm_no2)

# Heat Index - Ozone
cor_hi_oz <- cor(x = as.data.frame(data)[,heat_var], y =as.data.frame(data)[,ozone_var])
corrplot::corrplot(cor_hi_oz)

# Heat Index - NO2
cor_hi_no2 <- cor(x = as.data.frame(data)[,heat_var], y =as.data.frame(data)[,no2_var])
corrplot::corrplot(cor_hi_no2)


# DLNM --------------------------------------------------------------------
?crossbasis

### PM ###

cb1.pm_deg4 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=4))
pm_lag14_raw_deg4 <- clogit(case_day ~ cb1.pm_deg4 + strata(QID), 
                            data = data)

pm_lag14 = summary(pm_lag14_raw_deg4)
pred1.pm <- crosspred(cb1.pm_deg4, pm_lag14_raw_deg4, at=0:20, bylag=0.1, cumul=TRUE)

IQR_pm= round(IQR(data$pm_lag_0))
pred1.pm$allRRfit[as.character(IQR_pm)]
cbind(pred1.pm$allRRlow, pred1.pm$allRRhigh)[as.character(IQR_pm),]

plot(pred1.pm, "slices", var=IQR_pm, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main="Association with a IQR increase in PM2.5")
plot(pred1.pm, "slices", var=IQR_pm, col=2, cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association with a IQR increase in PM2.5")

### PM CONTROLLING FOR OTHERS ###

cb1.pm_deg3 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=3))
pm_lag14_raw_deg3 <- clogit(case_day ~ cb1.pm_deg3 + no2_lag_0 + heat_index_lag_0 + strata(QID), 
                            data = data)

pm_lag14 = summary(pm_lag14_raw_deg3)
pred1.pm <- crosspred(cb1.pm_deg3, pm_lag14_raw_deg3, at=0:20, bylag=0.1, cumul=TRUE)

IQR_pm= round(IQR(data$pm_lag_0))
pred1.pm$allRRfit[as.character(IQR_pm)]
cbind(pred1.pm$allRRlow, pred1.pm$allRRhigh)[as.character(IQR_pm),]

plot(pred1.pm, "slices", var=IQR_pm, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main="Association with a IQR increase in PM2.5 - control for HI and no2")
plot(pred1.pm, "slices", var=IQR_pm, col=2, cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association with a IQR increase in PM2.5 - controlling for ozone and no2")


### Heat index ###

cb1.hi_deg4 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=4))
hi_lag14_raw_deg4 <- clogit(case_day ~ cb1.hi_deg4 + strata(QID), 
                            data = data)

pred1.hi <- crosspred(cb1.hi_deg4, hi_lag14_raw_deg4, at=0:20, bylag=0.2, cumul=TRUE)

plot(pred1.hi, "slices", var=5, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main="Association with a 5-unit increase in heat index")
plot(pred1.hi, "slices", var=5, col=2, cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association with a 5-unit increase in heat index")

pred1.hi$allRRfit["5"]
cbind(pred1.hi$allRRlow, pred1.hi$allRRhigh)["5",]


### Heat index CONTROLLING FOR OTHER EXPOSURES ###

cb1.hi_deg4 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=4))
hi_lag14_raw_deg4 <- clogit(case_day ~ cb1.hi_deg4 + no2_lag_0 + pm_lag_0 + strata(QID), 
                            data = data)

pred1.hi <- crosspred(cb1.hi_deg4, hi_lag14_raw_deg4, at=0:20, bylag=0.2, cumul=TRUE)

plot(pred1.hi, "slices", var=5, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main="Association with a 5-unit increase in heat index - control for PM and no2")
plot(pred1.hi, "slices", var=5, col=2, cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association with a 5-unit increase in heat index - controlling for ozone and no2")

pred1.hi$allRRfit["5"]
cbind(pred1.hi$allRRlow, pred1.hi$allRRhigh)["5",]

### 2 Exposures ###

cb1.pm_deg4 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=4))
cb1.hi_deg3 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=3))

pm_hi_lag14 = clogit(case_day ~ cb1.pm_deg4 + cb1.hi_deg3 + strata(QID), 
              data = data)

pred2.pm <- crosspred(cb1.pm_deg4, pm_hi_lag14, at=0:20, bylag=0.1, cumul=TRUE)

plot(pred2.pm, "slices", var=IQR_pm, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main="Association with a IQR increase in PM2.5")

pred2.pm$allRRfit[as.character(IQR_pm)]
cbind(pred2.pm$allRRlow, pred2.pm$allRRhigh)[as.character(IQR_pm),]

pred2.hi <- crosspred(cb1.hi_deg3, pm_hi_lag14, at=0:20, bylag=0.2, cumul=TRUE)

plot(pred2.hi, "slices", var=5, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main="Association with a 5-unit increase in heat index")

pred2.hi$allRRfit["5"]
cbind(pred2.hi$allRRlow, pred2.hi$allRRhigh)["5",]





# Choice degree -----------------------------------------------------------

### PM ###
# Deg 3
cb1.pm_deg3 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=3))
pm_lag14_raw_deg3 <- clogit(case_day ~ cb1.pm_deg3 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg3)

# Deg 4
cb1.pm_deg4 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=4))
pm_lag14_raw_deg4 <- clogit(case_day ~ cb1.pm_deg4 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg4)

# Deg 5
cb1.pm_deg5 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=5))
pm_lag14_raw_deg5 <- clogit(case_day ~ cb1.pm_deg5 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg5)

# Deg 6
cb1.pm_deg6 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=6))
pm_lag14_raw_deg6 <- clogit(case_day ~ cb1.pm_deg6 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg6)

# BEST
AIC_pm = c(AIC(pm_lag14_raw_deg3), AIC(pm_lag14_raw_deg4), AIC(pm_lag14_raw_deg5), AIC(pm_lag14_raw_deg6))
which.min(AIC_pm) #df 4

### PM CONTROLLING FOR OTHERS ###
# Deg 3
cb1.pm_deg3 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=3))
pm_lag14_raw_deg3 <- clogit(case_day ~ cb1.pm_deg3 + no2_lag_0 + heat_index_lag_0 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg3)

# Deg 4
cb1.pm_deg4 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=4))
pm_lag14_raw_deg4 <- clogit(case_day ~ cb1.pm_deg4 + no2_lag_0 + heat_index_lag_0 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg4)

# Deg 5
cb1.pm_deg5 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=5))
pm_lag14_raw_deg5 <- clogit(case_day ~ cb1.pm_deg5 + no2_lag_0 + heat_index_lag_0 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg5)

# Deg 6
cb1.pm_deg6 <- crossbasis(data[,paste0("pm_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=6))
pm_lag14_raw_deg6 <- clogit(case_day ~ cb1.pm_deg6 + no2_lag_0 + heat_index_lag_0 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg6)

# BEST
AIC_pm = c(AIC(pm_lag14_raw_deg3), AIC(pm_lag14_raw_deg4), AIC(pm_lag14_raw_deg5), AIC(pm_lag14_raw_deg6))
which.min(AIC_pm) #df 3



### Heat index ###

# Deg 3 
cb1.hi_deg3 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=3))
hi_lag14_raw_deg3 <- clogit(case_day ~ cb1.hi_deg3 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg3)


# Deg 4
cb1.hi_deg4 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=4))
hi_lag14_raw_deg4 <- clogit(case_day ~ cb1.hi_deg4 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg4)


# Deg 5
cb1.hi_deg5 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=5))
hi_lag14_raw_deg5 <- clogit(case_day ~ cb1.hi_deg5 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg5)


# Deg 6
cb1.hi_deg6 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=6))
hi_lag14_raw_deg6 <- clogit(case_day ~ cb1.hi_deg6 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg6)

# BEST
AIC_hi = c(AIC(hi_lag14_raw_deg3), AIC(hi_lag14_raw_deg4), AIC(hi_lag14_raw_deg5), AIC(hi_lag14_raw_deg6))
which.min(AIC_hi) #df 4


### Heat index CONTROLLING FOR OTHER EXPOSURES ###

# Deg 3 
cb1.hi_deg3 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=3))
hi_lag14_raw_deg3 <- clogit(case_day ~ cb1.hi_deg3 + no2_lag_0 + pm_lag_0 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg3)


# Deg 4
cb1.hi_deg4 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=4))
hi_lag14_raw_deg4 <- clogit(case_day ~ cb1.hi_deg4 + no2_lag_0 + pm_lag_0 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg4)


# Deg 5
cb1.hi_deg5 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=5))
hi_lag14_raw_deg5 <- clogit(case_day ~ cb1.hi_deg5 + no2_lag_0 + pm_lag_0 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg5)


# Deg 6
cb1.hi_deg6 <- crossbasis(data[,paste0("heat_index_lag_",0:14)], lag=14, argvar=list(fun="lin"),
                          arglag=list(fun="poly",degree=6))
hi_lag14_raw_deg6 <- clogit(case_day ~ cb1.hi_deg6 + no2_lag_0 + pm_lag_0 + strata(QID), 
                            data = data)
AIC(pm_lag14_raw_deg6)

# BEST
AIC_hi = c(AIC(hi_lag14_raw_deg3), AIC(hi_lag14_raw_deg4), AIC(hi_lag14_raw_deg5), AIC(hi_lag14_raw_deg6))
which.min(AIC_hi) #df 4


#### 2 Exposures

mod1 = clogit(case_day ~ cb1.pm_deg3 + cb1.hi_deg3 + strata(QID), 
              data = data)
mod2 = clogit(case_day ~ cb1.pm_deg3 + cb1.hi_deg4 + strata(QID), 
              data = data)
mod3 = clogit(case_day ~ cb1.pm_deg4 + cb1.hi_deg3 + strata(QID), 
              data = data)
mod4 = clogit(case_day ~ cb1.pm_deg4 + cb1.hi_deg4 + strata(QID), 
              data = data)

AIC_2exp = c(AIC(mod1), AIC(mod2), AIC(mod3), AIC(mod4))
which.min(AIC_2exp) # mod3
