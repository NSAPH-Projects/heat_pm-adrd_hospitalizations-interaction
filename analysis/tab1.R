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
library(tidyverse)
library(magrittr)
library(sf)
library(raster)
library(gridExtra)
library(MetBrewer)
library(foreign)
library(tidyverse)
library(ggtext)  
library(tigris)
setwd("/n/dominici_nsaph_l3/Lab/projects/heat_pm-adrd_hospitalizations-interaction/code")

load("../data/scratch/cohort_lagged_dat.Rdata")

# data summary
data[case_day == 1, .N]
data[case_day == 1, .N, by = sex]
data[case_day == 1 & age <= 74, .N]
data[case_day == 1 & age >= 75 & age <= 84, .N]
data[case_day == 1 & age >= 85, .N]
data[case_day == 1, .N, by = dual]
data[case_day == 1, .N/data[case_day == 1, .N], by = dual]
data[case_day == 1, .N, by = race]

data[case_day == 1, .(median(pm_lag_0), sd(pm_lag_0))]
data[case_day == 1, .(median(orig_heat_index_lag_0), sd(orig_heat_index_lag_0))]

data[case_day == 0, .(median(pm_lag_0), sd(pm_lag_0))]
data[case_day == 0, .(median(orig_heat_index_lag_0), sd(orig_heat_index_lag_0))]

median(data$pm_lag_0)
quantile(data$pm_lag_0, c(0.25,0.75))

median(data$orig_heat_index_lag_0)
quantile(data$orig_heat_index_lag_0, c(0.25,0.75))

########### TABLE 1 ##############
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
label(data2$orig_heat_index_lag_0)  <-"Same-day heat index"
label(data2$pm_lag_0)  <-"Same-day PM2.5"


units(data2$age)       <- "years"
units(data2$agerange)       <- "years"
units(data2$orig_heat_index_lag_0) <-"°C"
units(data2$pm_lag_0) <-"μg/m3"

caption  <- "Table 1"

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.1f %%)", FREQ, PCT))))
}

my.table1<-table1(~ sex + agerange + race + orig_heat_index_lag_0 + pm_lag_0,
                  data=data2,
                  overall=c(left="Total"),
                  caption=caption,render.continuous=my.render.cont,
                  render.categorical=my.render.cat
)


## Figure 1 ------------------------------------------------------------------------------------------------

## Koppen
Koppen_counties <- read_csv("/n/dominici_nsaph_l3/Lab/projects/temperature-adrd-casecrossover/data/Koppen_counties.csv") %>%
  mutate(StCoFIPS = sprintf("%05d", StCoFIPS))

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

counties <- st_read("../data/tl_2016_us_county/tl_2016_us_county.shp") %>% 
  filter(!STATEFP %in% c( "02","15", "66", "72", "60", "69", "78")) %>%
  left_join(Koppen_counties, by = c("GEOID" = "StCoFIPS"))%>%
  st_transform(crs = 5070)


## Annual air pollution
zip_sf_list = read_rds("/n/dominici_nsaph_l3/Lab/projects/totalPM_smokePM-mortality-causal_analysis/data/input/smoke/zip_sf_list.rds")
total_pm = as.data.frame(read_rds("/n/dominici_nsaph_l3/Lab/projects/totalPM_smokePM-mortality-causal_analysis/data/input/smoke/total_pm.rds")) 

year = "2016"
zip_sf = zip_sf_list[[year]] %>%
  rename(zip = ZIP)

zip_total_pm_df = total_pm[total_pm$year == as.numeric(year),]

zip_sf %<>% 
  left_join(zip_total_pm_df)


## Plots
# First plot: PM2.5 levels in 2016
plot1 <- zip_sf %>%
  st_transform(crs = 5070) %>%
  st_as_sf() %>%
  st_simplify() %>%
  ggplot() +
  geom_sf(aes(fill = pm), lwd = 0, col = NA) + 
  scale_fill_viridis_c(na.value = "lightgrey",
                       limits=c(0,15), 
                       name = expression("PM"[2.5]*" ("*mu*" g/m"^3*")"))+
  theme_void() +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 26, family = "Arial", face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key.width = unit(1.5, 'cm'),
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  labs(title = "<b>(A) Annual average of PM<sub>2.5</sub> in 2016</b>")

ggsave("pm25_map.png", plot1, width = 10, height = 8, dpi = 300)

# Second plot: Köppen climate map 
plot2 <- counties %>%
  st_as_sf() %>%
  st_simplify() %>%
  filter(!is.na(Koppen)) %>%
  ggplot() +
  geom_sf(aes(fill = Koppen), linewidth = 0.2, color = "black") +
  scale_fill_manual(
    values = met.brewer("Hokusai1", n = 18),
    name = ""
  ) +
  guides(fill = guide_legend(nrow = 6)) +  # Specify 3 rows for the legend
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 56, family = "Arial", face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 26),
    legend.key.width = unit(0.8, "cm"),
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  ggtitle("(B) Köppen-Geiger climate sub-types")


ggsave("koppen_map.png", plot2, width = 22, height = 17, dpi=300)


