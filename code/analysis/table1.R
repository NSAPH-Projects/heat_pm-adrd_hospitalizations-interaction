# Required libraries
library(data.table)
library(lubridate)
library(weathermetrics)
library(dplyr)
library(fst)
library(viridis)
library(ggpubr)
library(table1)
library(survival)
library(ggplot2)
setwd("~/heat_pm-adrd_hospitalizations-interaction/code")

load("../data/scratch/case_day-crossover-exposure.Rdata")
data = as.data.table(data)

# data summary
data[case_day == 1, .N]
data[case_day == 1, .N, by = sex]
data[case_day == 1 & age <= 74, .N]
data[case_day == 1 & age >= 75 & age <= 84, .N]
data[case_day == 1 & age >= 85, .N]
data[case_day == 1, .N, by = dual]
data[case_day == 1, .N, by = race]

data[case_day == 1, .(mean(pm25), sd(pm25))]
data[case_day == 1, .(mean(no2), sd(no2))]
data[case_day == 1, .(mean(ozone), sd(ozone))]
data[case_day == 1, .(mean(heat_index), sd(heat_index))]

data[case_day == 0, .(mean(pm25), sd(pm25))]
data[case_day == 0, .(mean(no2), sd(no2))]
data[case_day == 0, .(mean(ozone), sd(ozone))]
data[case_day == 0, .(mean(heat_index), sd(heat_index))]


########### TABLE 1 ##############
names(data2)
data2<-subset(data,case_day==1)

#### CREATING TABLE 1######
data2$sex <- 
  factor(data2$sex, levels=c("1","2"),
         labels=c("Male", 
                  "Female"))

data2$agerange = as.factor(ifelse(data2$AGE %in% c(65:69),1, 
                           ifelse(data2$AGE %in% c(70:74),2, 
                           ifelse(data2$AGE %in% c(75:79),3,  
                           ifelse(data2$AGE %in% c(80:84),4, 
                           ifelse(data2$AGE %in% c(85:89),5,
                           ifelse(data2$AGE %in% c(90:94),6,7)))))))

data2$agerange <- 
  factor(data2$agerange, levels=c("1","2","3","4","5","6","7"),
         labels=c("65 - 69", 
                  "70 - 74", 
                  "75 - 79", 
                  "80 - 84", 
                  "85 - 89", 
                  "90 - 94", 
                  "95 + " ))

data2$race <- 
  factor(data2$race, levels=c("1","2","3","4","5","6","0"),
         labels=c("White", 
                  "Black", 
                  "Other", 
                  "Asian", 
                  "Hispanic", 
                  "North American Native", 
                  "Unknown" ))

label(data2$sex)       <- "Sex"
label(data2$race)       <- "Race"
label(data2$agerange)       <- "Age"
label(data2$heat_index)  <-"Same-day heat index"
label(data2$ozone)  <-"Same-day ozone"
label(data2$no2)  <-"Same-day NO2"
label(data2$pm25)  <-"Same-day PM2.5"


units(data2$age)       <- "years"
units(data2$agerange)       <- "years"
units(data2$heat_index) <-"°C"
units(data2$ozone) <-"ppb"
units(data2$no2) <-"ppb"
units(data2$pm25) <-"μg/m3"


caption  <- "Table 1"



my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

my.table1<-table1(~ sex + agerange + race + heat_index + ozone + no2 + pm25,
                  data=data2,
                  overall=c(left="Total"),
                  caption=caption,render.continuous=my.render.cont,
                  render.categorical=my.render.cat
)





## Exposure distribution ------------------------------------------------------------------------------------------------

p1 <- data %>%
  ggplot( aes( x=pm25)) +
  geom_boxplot(color="#69b3a2") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
   ggtitle("Distribution of PM2.5 ") 
p1

summary(data$pm25)
IQR(data$pm25)

# HI
p2 <- data %>%
  ggplot( aes( x=heat_index)) +
  geom_boxplot(color="#69b3a2") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  ggtitle("Distribution of heat index ") 
p2

summary(data$heat_index)
IQR(data$heat_index)

# NO2
p3 <- data %>%
  ggplot( aes( x=no2)) +
  geom_boxplot(color="#69b3a2") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  ggtitle("Distribution of NO2") 
p3

summary(data$no2)
IQR(data$no2)

# ozone
p4 <- data %>%
  ggplot( aes( x=ozone)) +
  geom_boxplot(color="#69b3a2") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  ggtitle("Distribution of ozone") 
p4

summary(data$ozone)
IQR(data$ozone)

## Exposure correlation -----------------------------------------------------------------------------------------
cor(data[,c("pm25", "heat_index", "no2", "ozone")])
corrplot::corrplot(cor(data[,c("pm25", "heat_index", "no2", "ozone")]))
