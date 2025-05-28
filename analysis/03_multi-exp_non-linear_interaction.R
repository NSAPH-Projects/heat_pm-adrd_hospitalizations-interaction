library(survival)
library(splines)
library(ggplot2)
library(data.table)
library(lubridate)
library(weathermetrics)
library(dplyr)
library(fst)
library(splines)
library(viridis)
library(ggpubr)
library(dlnm)
library(mgcv)
library(plotly)
setwd("/n/dominici_nsaph_l3/Lab/projects/heat_pm-adrd_hospitalizations-interaction/code")

source("analysis/utils.R")

# Load data
load("../data/scratch/cohort_lagged_dat.Rdata")

pm_99perc = quantile(data$pm_lag_0, 0.99)
data = data %>%
  # mutate_at(vars(pm_lag_0, pm_lag_1, pm_lag_2), ~ ifelse(. > pm_99perc, pm_99perc, .))%>%
  mutate(hi3avg = (heat_index_lag_0 + heat_index_lag_1 + heat_index_lag_2 )/3)%>%
  mutate(pm3avg = (pm_lag_0 + pm_lag_1 + pm_lag_2 )/3) 

# Add joint basis to the data
basis_el  <- joint_basis(data$heat_index_lag_0, data$pm_lag_0)
b1 = basis_el$b1
b2 = basis_el$b2
basis_matrix = basis_el$basis
colnames(basis_matrix) <- c(paste0("b1_", 1:3), paste0("b2_", 1:3), paste0("int_", 1:9))
data <- cbind(data, basis_matrix)

# Model
model <- clogit(case_day ~ b1_1 + b1_2 + b1_3 + b2_1 + b2_2 + b2_3 + 
                  int_1 + int_2 + int_3 + int_4 +int_5 + int_6 + int_7 + int_8 + int_9 +
                  strata(QID), data=data)
out = summary(model)
AIC(model)

# Coefficients and variance-covariance matrix
coefficients <- out$coefficients[,1]
vcov_matrix <- vcov(model)

# Grid for effect prediction
exp1_range <- seq(quantile(data$heat_index_lag_0, 0.01), quantile(data$heat_index_lag_0, 0.99), length.out = 500)
exp2_range <- seq(quantile(data$pm_lag_0, 0.01), pm_99perc, length.out = 500)
grid <- expand.grid(heat_index_lag_0 = exp1_range, pm_lag_0 = exp2_range)
basis_matrix_pred <- project_basis(grid$heat_index_lag_0, grid$pm_lag_0, b1, b2)
colnames(basis_matrix_pred) <- c(paste0("b1_", 1:3), paste0("b2_", 1:3), paste0("int_", 1:9))

# Reference index
ref <- c(0.5, 5)  
idx <- which.min(rowSums(abs(sweep(grid, 2, ref))))

# Prediction
basis_matrix_ref <- basis_matrix_pred[idx, , drop=FALSE]  
basis_diff <- sweep(basis_matrix_pred, 2, basis_matrix_ref)
log_odds_ratio <- basis_diff %*% coefficients
var_log_odds_ratio <- rowSums((basis_diff %*% vcov_matrix) * basis_diff) # Variance of log odds ratio

# Confidence intervals for log odds ratio
z_critical <- 1.96  
log_odds_ratio_lower <- log_odds_ratio - z_critical * sqrt(var_log_odds_ratio)
log_odds_ratio_upper <- log_odds_ratio + z_critical * sqrt(var_log_odds_ratio)

# Combine results
results_lu <- data.frame(
  odds_ratio = exp(log_odds_ratio),
  odds_ratio_lower = exp(log_odds_ratio_lower),
  odds_ratio_upper = exp(log_odds_ratio_upper)
)

data_exp <- cbind(grid, results_lu)

# Compare for set increase
ref_2 <- c(0.99, 10)  
idx_2 <- which.min(rowSums(abs(sweep(grid, 2, ref_2))))
data_exp[idx_2, ]

# 3D Plot -----------------------------------------------------------------
ggplot(data_exp, aes(x = heat_index_lag_0, y = pm_lag_0, z = odds_ratio)) +
  geom_contour_filled() +
  scale_fill_viridis_d() +
  labs(title = "Odds Ratio Plot wrt reference level (0.5, 5)",
       x = "Heat Index", y = "PM 2.5", fill = "Odds Ratio")+
  theme_minimal()

# Reshape the data into matrices for the 3D plot
x_unique <- unique(data_exp$heat_index_lag_0)
y_unique <- unique(data_exp$pm_lag_0)
z_matrix <- matrix(data_exp$odds_ratio, 
                   nrow = length(y_unique),
                   ncol = length(x_unique),
                   byrow = TRUE)

p <- plot_ly(
  x = x_unique,
  y = y_unique,
  z = z_matrix,
  type = "surface"
) %>%
  add_surface(
    x = x_unique,
    y = y_unique,
    z = matrix(1, nrow = length(y_unique), ncol = length(x_unique)),
    showscale = FALSE,
    opacity = 0.5
  ) %>%
  layout(
    scene = list(
      xaxis = list(
        title = list(
          text = "Heat index percentile",
          font = list(size = 18, family = "Arial")
        ),
        tickfont = list(size = 14, family = "Arial")
      ),
      yaxis = list(
        title = list(
          text = "PM<sub>2.5</sub> (µg/m<sup>3</sup>)",
          font = list(size = 18, family = "Arial")
        ),
        tickfont = list(size = 14, family = "Arial")
      ),
      zaxis = list(
        title = list(
          text = "OR",
          font = list(size = 18, family = "Arial")
        ),
        tickfont = list(size = 14, family = "Arial")
      )
    ),
    title = list(
      text = "<b>(A) Odds ratio compared to reference exposure level</b>",
      font = list(size = 25, family = "Arial")
    ),
    margin = list(t = 100)
  ) %>%
  colorbar(
    title = list(
      text = "OR",
      font = list(size = 18, family = "Arial")
    ),
    tickfont = list(size = 14, family = "Arial"),
    len = 0.5,
    x = 0.85,
    thickness = 20
  )

p


# For PM2.5 slices
pm_values <- c(5, 10, 15)
pm_indices <- sapply(pm_values, function(x) which.min(abs(y_unique - x)))
selected_pm <- y_unique[pm_indices]

data_pm_slices <- do.call(rbind, lapply(selected_pm, function(pm) {
  data_exp[data_exp$pm_lag_0 == pm,]
}))
data_pm_slices$pm_level <- factor(round(data_pm_slices$pm_lag_0, 1))

# For Heat Index slices
hi_values <- c(0.5, 0.99)
hi_indices <- sapply(hi_values, function(x) which.min(abs(x_unique - x)))
selected_hi <- x_unique[hi_indices]

data_hi_slices <- do.call(rbind, lapply(selected_hi, function(hi) {
  data_exp[data_exp$heat_index_lag_0 == hi,]
}))
data_hi_slices$hi_level <- factor(round(data_hi_slices$heat_index_lag_0, 2))


# PM2.5 slices plot
pm_slice_plot <- ggplot(data_pm_slices, aes(x = heat_index_lag_0, y = odds_ratio, color = pm_level)) +
  geom_line() +
  # geom_ribbon(aes(ymin = odds_ratio_lower, ymax = odds_ratio_upper, fill = pm_level), alpha = 0.1) +
  labs(title = expression(bold("(B) Slices for different PM"[2.5]*" levels")),
       x = "Heat index percentile", 
       y = "OR",
       color = expression("PM"[2.5]*" ("*mu*" g/m"^3*")"),  
       fill = expression("PM"[2.5]*" ("*mu*" g/m"^3*")")) +
  theme_minimal() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  xlim(c(0.5,1))+
  ylim(c(0.98,1.04))+
  theme_minimal() +
  scale_color_discrete(labels = c("5", "10", "15"))  # simplified numeric labels

# Heat Index slices plot 
hi_slice_plot <- ggplot(data_hi_slices, aes(x = pm_lag_0, y = odds_ratio, color = hi_level)) +
  geom_line() +
  # geom_ribbon(aes(ymin = odds_ratio_lower, ymax = odds_ratio_upper, fill = hi_level), alpha = 0.2) +
  labs(title = expression(bold("(C) Slices for different heat index percentile levels")),
       x = expression("PM"[2.5]*" ("*mu*" g/m"^3*")"), 
       y = "OR",
       color = "Heat index \npercentile",
       fill = "Heat index \npercentile") +
  theme_minimal() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  theme_minimal() +
  scale_color_discrete(labels = c("0.5", "0.99"))  


ggarrange(pm_slice_plot, hi_slice_plot, ncol = 2)

combined_plot <- ggarrange(pm_slice_plot, hi_slice_plot, ncol = 2)
ggsave(filename = "slices_plot.png", plot = combined_plot, width = 12, height = 6, dpi = 300)

ggsave(filename = "Figure_8.png", plot = combined_plot, width = 12, height = 6, dpi = 300)

