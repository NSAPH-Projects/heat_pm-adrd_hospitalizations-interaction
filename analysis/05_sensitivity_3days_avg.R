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

# Single exposure models --------------------------------------------------

## Heat index
cb <- crossbasis(data[,paste0("hi3avg")],
                 lag = 0, 
                 argvar = list(fun = "bs", df = 4))
model <- clogit(case_day ~ cb + strata(QID), 
                data)
cp_single_hi <- crosspred(cb, model, at = 1:999/1000, cen = 0.5, cumul = TRUE)
plot(cp_single_hi, main = expression(bold("Single exposure - HI: Odds ratio wrt 50% percentile")), xlab = "Average heat index percentile" )

round(cp_single_hi$allRRfit["0.99"],3)
round(cbind(cp_single_hi$allRRlow, cp_single_hi$allRRhigh)["0.99",],3)

## linear regression on heat index
mod <- clogit(case_day ~ hi3avg + strata(QID), data = data)
est <- coef(mod)[['hi3avg']]; se <- sqrt(vcov(mod)[1,1])
print(summary(mod))
cat("print OR for 0.49 increase in heat index percentile and its 95CI \n")
round(exp(est*0.49),3)
round(exp((est-qnorm(.975)*se)*0.49),3)
round(exp((est+qnorm(.975)*se)*0.49),3)

## PM2.5
cb <- crossbasis(data[,paste0("pm3avg")],
                 lag = 0, 
                 argvar = list(fun = "bs", df = 4))
model <- clogit(case_day ~ cb + strata(QID), 
                data)

cp_single_pm <- crosspred(cb, model, at=seq(0, 36, by=0.1),cen = 5,  cumul = TRUE)

plot(cp_single_pm, main = expression(bold("Single exposure - PM"[2.5]) ~ bold(": Odds ratio wrt 5") ~ bold(mu*"g/m"^3)), xlab = expression("Average PM"[2.5]) )

round(cp_single_pm$allRRfit["10"],3)
round(cbind(cp_single_pm$allRRlow, cp_single_pm$allRRhigh)["10",],3)

cp_single_pm <- crosspred(cb, model, at=seq(0, 36, by=0.1),cen = 10,  cumul = TRUE)

round(cp_single_pm$allRRfit["15"],3)
round(cbind(cp_single_pm$allRRlow, cp_single_pm$allRRhigh)["15",],3)

## linear regression on heat index
mod <- clogit(case_day ~ pm3avg + strata(QID), data = data)
print(summary(mod))
est <- coef(mod)[['pm3avg']]; se <- sqrt(vcov(mod)[1,1])

cat("print OR for 5 units increase in PM2.5 and its 95CI \n")
round(exp(est*5),3)
round(exp((est-qnorm(.975)*se)*5),3)
round(exp((est+qnorm(.975)*se)*5),3)


# Joint-exposure models --------------------------------------------------

## No interaction

#Linear
lag0_raw <- clogit(case_day ~ pm3avg + hi3avg + strata(QID), 
                   data = data)
exp_lag0 = summary(lag0_raw)

# PM
round(exp(5 * exp_lag0$coefficients[1]),3)
round(c(exp(5 * (exp_lag0$coefficients[1] - 1.96 * exp_lag0$coefficients[1,3])), exp(5 * (exp_lag0$coefficients[1] + 1.96 * exp_lag0$coefficients[1,3]))),3)

# HI
round(exp(0.49*exp_lag0$coefficients[2]),3)
round(c(exp( 0.49*(exp_lag0$coefficients[2] - 1.96 * exp_lag0$coefficients[2,3])), exp(0.49*(exp_lag0$coefficients[2] + 1.96 * exp_lag0$coefficients[2,3]))),3)


# Non linear
cb_heat <- crossbasis(data[,"hi3avg"],
                      lag = 0, 
                      argvar = list(fun = "bs", df = 4))

cb_pm <- crossbasis(data[,"pm3avg"],
                    lag = 0, 
                    argvar = list(fun = "bs", df = 4))

model <- clogit(case_day ~ cb_heat + cb_pm + strata(QID), 
                data)
cp_heat <- crosspred(cb_heat, model, at = 1:999/1000, cen = 0.5, cumul = TRUE)
plot(cp_heat, main = paste0("HI: Odds ratio wrt 50% percentile"), xlab = "Average heat index percentile")

round(cp_heat$allRRfit["0.99"],3)
round(cbind(cp_heat$allRRlow, cp_heat$allRRhigh)["0.99",],3)


cp_pm <- crosspred(cb_pm, model, at=seq(0, 36, by=0.1),cen = 5,  cumul = TRUE)
plot(cp_pm, main = expression(bold("PM"[2.5]) ~ bold(": Odds ratio wrt 5") ~ bold(mu*"g/m"^3)), 
     xlab = expression("Average PM"[2.5]) )

round(cp_pm$allRRfit["10"],3)
round(cbind(cp_pm$allRRlow, cp_pm$allRRhigh)["10",],3)

cp_pm <- crosspred(cb_pm, model, at=seq(0, 36, by=0.1),cen = 10,  cumul = TRUE)

round(cp_pm$allRRfit["15"],3)
round(cbind(cp_pm$allRRlow, cp_pm$allRRhigh)["15",],3)



png("Figure_5.png", width = 14, height = 8, units = "in", res = 300)

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

# Plot 1: Heat index - Single exposure
plot(cp_single_hi, main = paste0("Heat index - Odds ratio for changes from median"), xlab = "Average heat index percentile", ylab = "OR")

# Plot 2: PM2.5 - Single exposure
plot(cp_single_pm, main = expression(bold("PM"[2.5]) ~ bold("- Odds ratio for changes from 5") ~ bold(mu*"g/m"^3)), xlab = expression("Average PM"[2.5]*" ("*mu*"g/m"^3*")"), ylab = "OR")


# Plot 3: Heat index - Joint exposure
plot(cp_heat, main = paste0("Heat index - Odds ratio for changes from median"), xlab = "Average heat index percentile", ylab = "OR")

# Plot 4: PM2.5 - Joint exposure
plot(cp_pm, main = expression(bold("PM"[2.5]) ~ bold("- Odds ratio for changes from 5") ~ bold(mu*"g/m"^3)), 
     xlab = expression("Average PM"[2.5]*" ("*mu*"g/m"^3*")"), ylab = "OR")

# Add section labels
mtext("A) Single exposure models", side = 3, line = 0.5, outer = TRUE, adj = 0, font = 2)
mtext("B) Joint-exposure model",  side = 3, line = -30, outer = TRUE, adj = 0, font = 2)

# Close device
dev.off()

## Interaction
lag0_inter_raw <- clogit(case_day ~ pm3avg + hi3avg + pm3avg*hi3avg + strata(QID), 
                         data = data)
exp_lag0 = summary(lag0_inter_raw)

# PM
round(exp(5 * exp_lag0$coefficients[1]),3)
round(c(exp(5 * (exp_lag0$coefficients[1] - 1.96 * exp_lag0$coefficients[1,3])), exp(5 * (exp_lag0$coefficients[1] + 1.96 * exp_lag0$coefficients[1,3]))),3)

# HI
round(exp(0.49*exp_lag0$coefficients[2]),3)
round(c(exp( 0.49*(exp_lag0$coefficients[2] - 1.96 * exp_lag0$coefficients[2,3])), exp(0.49*(exp_lag0$coefficients[2] + 1.96 * exp_lag0$coefficients[2,3]))),3)

# Interaction term
round(exp(5 * 0.49 * exp_lag0$coefficients[3]),3)
round(c(exp(5 * 0.49 * (exp_lag0$coefficients[3] - 1.96 * exp_lag0$coefficients[3,3])), exp(5 * 0.49 * (exp_lag0$coefficients[3] + 1.96 * exp_lag0$coefficients[3,3]))),3)



# Interaction heat map
grids <- expand.grid(PM = seq(quantile(data$pm3avg, 0.01), quantile(data$pm3avg, 0.99), length.out = 200), HI = seq(quantile(data$hi3avg, 0.01), quantile(data$hi3avg, 0.99), length.out = 200))
grids$PM_med <- grids$PM - 5
grids$HI_med  <- grids$HI - median(data$hi3avg) 
grids$Value <- exp(grids$PM_med*exp_lag0$coefficients[1] + grids$HI_med*exp_lag0$coefficients[2] + grids$PM_med * grids$HI_med * exp_lag0$coefficients[3])


# Interaction heat map
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
      xaxis = list(title = "Average heat index"),
      yaxis = list(title = "Average PM<sub>2.5</sub>"), 
      zaxis = list(title = "Odds ratio")
    ),
    title = list(
      text = "Odds ratio compared to HI:0.5 and PM<sub>2.5</sub>:5",  
      font = list(size = 15)  
    ),
    margin = list(t = 100)  # Increase the top margin to add more white space above the title
  ) %>%
  colorbar(title = "OR")

p

## Non linear interaction

# Add joint basis to the data
basis_el  <- joint_basis(data$hi3avg, data$pm3avg)
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

# Coefficients and variance-covariance matrix
coefficients <- out$coefficients[,1]
vcov_matrix <- vcov(model)

# 3D Plot -----------------------------------------------------------------
# Grid for effect prediction
exp1_range <- seq(quantile(data$hi3avg, 0.01), quantile(data$hi3avg, 0.999), length.out = 700)
exp2_range <- seq(quantile(data$pm3avg, 0.01), pm_99perc, length.out = 700)
grid <- expand.grid(hi3avg = exp1_range, pm3avg = exp2_range)
basis_matrix_pred <- project_basis(grid$hi3avg, grid$pm3avg, b1, b2)
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
round(data_exp[idx_2, ],3)

# Plot
ggplot(data_exp, aes(x = hi3avg, y = pm3avg, z = odds_ratio)) +
  geom_contour_filled() +
  scale_fill_viridis_d() +
  labs(title = "Odds Ratio Plot wrt reference level (0.5, 5)",
       x = "Heat Index", y = "PM 2.5", fill = "Odds Ratio")+
  theme_minimal()

# Reshape the data into matrices for the 3D plot
x_unique <- unique(data_exp$hi3avg)
y_unique <- unique(data_exp$pm3avg)
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
          text = "Average heat index percentile",
          font = list(size = 18, family = "Arial")
        ),
        tickfont = list(size = 14, family = "Arial")
      ),
      yaxis = list(
        title = list(
          text = "Average PM<sub>2.5</sub> (µg/m<sup>3</sup>)",
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
  data_exp[data_exp$pm3avg == pm,]
}))
data_pm_slices$pm_level <- factor(round(data_pm_slices$pm3avg, 1))

# For Heat Index slices
hi_values <- c(0.5, 0.99)
hi_indices <- sapply(hi_values, function(x) which.min(abs(x_unique - x)))
selected_hi <- x_unique[hi_indices]

data_hi_slices <- do.call(rbind, lapply(selected_hi, function(hi) {
  data_exp[data_exp$hi3avg == hi,]
}))
data_hi_slices$hi_level <- factor(round(data_hi_slices$hi3avg, 2))

# PM2.5 slices plot
pm_slice_plot <- ggplot(data_pm_slices, aes(x = hi3avg, y = odds_ratio, color = pm_level)) +
  geom_line() +
  labs(title = expression(bold("(B) Slices for different PM"[2.5]*" levels")),
       x = "Average heat index percentile", 
       y = "OR",
       color = expression("Average PM"[2.5]*" ("*mu*" g/m"^3*")"),  # simplified legend title
       fill = expression("Average PM"[2.5]*" ("*mu*" g/m"^3*")")) +
  theme_minimal() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  xlim(c(0.5,1))+
  ylim(c(0.98,1.04))+
  theme_minimal() +
  scale_color_discrete(labels = c("5", "10", "15"))  # simplified numeric labels

# Heat Index slices plot 
hi_slice_plot <- ggplot(data_hi_slices, aes(x = pm3avg, y = odds_ratio, color = hi_level)) +
  geom_line() +
  labs(title = expression(bold("(C) Slices for different heat index percentile levels")),
       x = expression("Average PM"[2.5]*" ("*mu*" g/m"^3*")"), 
       y = "OR",
       color = "Average heat \nindex percentile",
       fill = "Average heat \nindex percentile") +
  theme_minimal() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  theme_minimal() +
  scale_color_discrete(labels = c("0.5", "0.99"))  


# To display both plots side by side
ggarrange(pm_slice_plot, hi_slice_plot, ncol = 2)

combined_plot <- ggarrange(pm_slice_plot, hi_slice_plot, ncol = 2)
ggsave(filename = "slices_plot_2.png", plot = combined_plot, width = 12, height = 6, dpi = 300)

##
# PM2.5 slices plot
pm_slice_plot <- ggplot(data_pm_slices, aes(x = hi3avg, y = odds_ratio, color = pm_level)) +
  geom_line() +
  geom_ribbon(aes(ymin = odds_ratio_lower, ymax = odds_ratio_upper, fill = pm_level), alpha = 0.1) +
  labs(title = expression(bold("Slices for different PM"[2.5]*" levels")),
       x = "Average heat index percentile", 
       y = "OR",
       color = expression("Average PM"[2.5]*" ("*mu*" g/m"^3*")"),  # simplified legend title
       fill = expression("Average PM"[2.5]*" ("*mu*" g/m"^3*")")) +
  theme_minimal() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  theme_minimal() +
  scale_color_discrete(labels = c("5", "10", "15"))  # simplified numeric labels

# Heat Index slices plot 
hi_slice_plot <- ggplot(data_hi_slices, aes(x = pm3avg, y = odds_ratio, color = hi_level)) +
  geom_line() +
  geom_ribbon(aes(ymin = odds_ratio_lower, ymax = odds_ratio_upper, fill = hi_level), alpha = 0.2) +
  labs(title = expression(bold("Slices for different heat index percentile levels")),
       x = expression("Average PM"[2.5]*" ("*mu*" g/m"^3*")"), 
       y = "OR",
       color = "Average heat \nindex percentile",
       fill = "Average heat \nindex percentile") +
  theme_minimal() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  theme_minimal() +
  scale_color_discrete(labels = c("0.5", "0.99"))  

combined_plot <- ggarrange(pm_slice_plot, hi_slice_plot, ncol = 2)
ggsave(filename = "Figure_9.png", plot = combined_plot, width = 12, height = 6, dpi = 300)



## Non linear dependence ----------------

# PM ~ HI
model_pm = lm(pm3avg~bs(hi3avg,4), data = data)
grid <- data.frame(hi3avg = seq(min(data$hi3avg), max(data$hi3avg), length.out = 200))

# Predict values with confidence intervals
predictions <- predict(model_pm, newdata = grid, interval = "confidence")
results <- cbind(grid, predictions)
grid$hi3avg[which.min(abs(grid$hi3avg - .5))]

results[which.min(abs(grid$hi3avg - .5)),]
results[which.min(abs(grid$hi3avg - .99)),]


exp_pm <- ggplot(results, aes(x = hi3avg, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  labs(
    title = expression(bold("(A) Estimated PM"[2.5]*" based on heat index level")),
    x = "Average heat index percentile",
    y = expression("Estimated PM"[2.5]*" ("*mu*" g/m"^3*")")
  ) +
  theme_minimal()

# Add joint basis to the data
basis_el  <- joint_basis(data$hi3avg, data$pm3avg)
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
  # Simulate pm3avg using model_pm
  pm_new_sim <- as.numeric(model.matrix(~bs(grid$hi3avg, 4)) %*% sim_coefs_pm[i, ])
  grid_sim <- data.frame(hi3avg = grid$hi3avg, pm3avg = pm_new_sim)
  
  # Project basis for simulated grid
  basis_matrix_pred_sim <- project_basis(grid_sim$hi3avg, grid_sim$pm3avg, b1, b2)
  colnames(basis_matrix_pred_sim) <- c(paste0("b1_", 1:3), paste0("b2_", 1:3), paste0("int_", 1:9))
  
  # Compute log odds ratio using simulated clogit coefficients
  log_odds_sim <- basis_matrix_pred_sim %*% sim_coefs_clogit[i, ]
  
  # Normalize odds ratios to the reference level
  ref_idx <- which.min(abs(grid_sim$hi3avg - 0.5))
  odds_sim <- exp(log_odds_sim)
  sim_odds_ratios[i, ] <- odds_sim / odds_sim[ref_idx]
}

# Compute mean and 95% confidence intervals for odds ratio
odds_ratio_mean <- colMeans(sim_odds_ratios)
odds_ratio_lower <- apply(sim_odds_ratios, 2, function(x) quantile(x, 0.025))
odds_ratio_upper <- apply(sim_odds_ratios, 2, function(x) quantile(x, 0.975))

# Combine results into a data frame
data_exp <- data.frame(
  hi3avg = grid$hi3avg,
  odds_ratio = odds_ratio_mean,
  odds_ratio_lower = odds_ratio_lower,
  odds_ratio_upper = odds_ratio_upper
)

idx <- which.min(abs(data_exp$hi3avg - 0.99))
data_exp[idx,]

# Plot results
heat_index_plot <- ggplot(data_exp, aes(x = hi3avg)) +
  geom_line(aes(y = odds_ratio), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = odds_ratio_lower, ymax = odds_ratio_upper), fill = "blue", alpha = 0.2) +
  labs(
    title = expression(bold("(B) Odds ratio for changes from median average heat index")),
    x = "Average heat index percentile",
    y = "OR"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

combined_plot <- ggarrange(exp_pm, heat_index_plot, ncol = 2)

# Save the combined plot 
ggsave(filename = "Figure_7.pdf", plot = combined_plot, width = 16, height = 6)
ggsave(filename = "Figure_7.png", plot = combined_plot, width = 16, height = 6, dpi = 300)
