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
set.seed(123)  # For reproducibility

# Load data
load("../data/scratch/cohort_lagged_dat.Rdata")

pm_99perc = quantile(data$pm_lag_0, 0.99)
data = data %>%
  # mutate_at(vars(pm_lag_0, pm_lag_1, pm_lag_2), ~ ifelse(. > pm_99perc, pm_99perc, .))%>%
  mutate(hi3avg = (heat_index_lag_0 + heat_index_lag_1 + heat_index_lag_2 )/3)%>%
  mutate(pm3avg = (pm_lag_0 + pm_lag_1 + pm_lag_2 )/3) 

# PM ~ HI model
model_pm = lm(pm_lag_0~bs(heat_index_lag_0,4), data = data)
grid <- data.frame(heat_index_lag_0 = seq(min(data$heat_index_lag_0), max(data$heat_index_lag_0), length.out = 500))

AIC(model_pm)

# Predict values with confidence intervals
predictions <- predict(model_pm, newdata = grid, interval = "confidence")
results <- cbind(grid, predictions)
grid$heat_index_lag_0[which.min(abs(grid$heat_index_lag_0 - .5))]

results[which.min(abs(grid$heat_index_lag_0 - .5)),]
results[which.min(abs(grid$heat_index_lag_0 - .99)),]

exp_pm <- ggplot(results, aes(x = heat_index_lag_0, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  labs(
    title = expression(bold("(A) Estimated PM"[2.5]*" based on heat index level")),
    x = "Heat index percentile",
    y = expression("Estimated PM"[2.5]*" ("*mu*" g/m"^3*")")
  ) +
  theme_minimal()


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

# Number of simulations
n_sim <- 5000

# Extract variance-covariance matrices
vcov_pm <- vcov(model_pm)  # For model_pm
vcov_clogit <- vcov(model)  # For clogit model

# Generate simulated coefficients for model_pm
sim_coefs_pm <- MASS::mvrnorm(n_sim, coef(model_pm), vcov_pm)

# Generate simulated coefficients for clogit model
sim_coefs_clogit <- MASS::mvrnorm(n_sim, coef(model), vcov_clogit)

# Initialize storage for simulated odds ratios
sim_odds_ratios <- matrix(NA, nrow = n_sim, ncol = nrow(grid))
  
# Simulate predictions and odds ratios
for (i in 1:n_sim) {
  # Simulate pm_lag_0 using model_pm
  pm_new_sim <- as.numeric(model.matrix(~bs(grid$heat_index_lag_0, 4)) %*% sim_coefs_pm[i, ])
  grid_sim <- data.frame(heat_index_lag_0 = grid$heat_index_lag_0, pm_lag_0 = pm_new_sim)
  
  # Project basis for simulated grid
  basis_matrix_pred_sim <- project_basis(grid_sim$heat_index_lag_0, grid_sim$pm_lag_0, b1, b2)
  colnames(basis_matrix_pred_sim) <- c(paste0("b1_", 1:3), paste0("b2_", 1:3), paste0("int_", 1:9))
  
  # Compute log odds ratio using simulated clogit coefficients
  log_odds_sim <- basis_matrix_pred_sim %*% sim_coefs_clogit[i, ]
  
  # Normalize odds ratios to the reference level
  ref_idx <- which.min(abs(grid_sim$heat_index_lag_0 - 0.5))
  odds_sim <- exp(log_odds_sim)
  sim_odds_ratios[i, ] <- odds_sim / odds_sim[ref_idx]
}

# Compute mean and 95% confidence intervals for odds ratio
odds_ratio_mean <- colMeans(sim_odds_ratios)
odds_ratio_lower <- apply(sim_odds_ratios, 2, function(x) quantile(x, 0.025))
odds_ratio_upper <- apply(sim_odds_ratios, 2, function(x) quantile(x, 0.975))

# Combine results into a data frame
data_exp <- data.frame(
  heat_index_lag_0 = grid$heat_index_lag_0,
  odds_ratio = odds_ratio_mean,
  odds_ratio_lower = odds_ratio_lower,
  odds_ratio_upper = odds_ratio_upper
)
idx <- which.min(abs(data_exp$heat_index_lag_0 - 0.99))
data_exp[idx,]

# Plot results
heat_index_plot <- ggplot(data_exp, aes(x = heat_index_lag_0)) +
  geom_line(aes(y = odds_ratio), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = odds_ratio_lower, ymax = odds_ratio_upper), fill = "blue", alpha = 0.2) +
  labs(
    title = expression(bold("(B) Odds ratio for changes from median heat index")),
    x = "Heat index percentile",
    y = "OR"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

combined_plot <- ggarrange(exp_pm, heat_index_plot, ncol = 2)

# Save the combined plot 
ggsave(filename = "Figure_4.pdf", plot = combined_plot, width = 16, height = 6)
ggsave(filename = "Figure_4.png", plot = combined_plot, width = 16, height = 6, dpi = 300)
