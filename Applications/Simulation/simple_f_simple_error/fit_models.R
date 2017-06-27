library(devtools)
install_github("opardo/GPDPQuantReg")

library(GPDPQuantReg)
library(ggplot2)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(R.utils)

# Define local_path where the repo lives (user dependent)
local_path <- "/Users/opardo/Documents/Projects/Personal/"

# Import utils
source(paste0(local_path,"Thesis/Applications/Simulation/utils.R"), local = TRUE)

# Setwd for simple function and simple error folder
setwd(paste0(local_path,"Thesis/Applications/Simulation/simple_f_simple_error/"))

set.seed(2017)

# GENERATE DATA

# Define base function
f_x <- function(x) return((1/40) * x^2 - (1/20) * x - 2)

# Define error function and inverse error function
error <- function(m) rnorm(m, 0, 1)
qerror <- function(p) qnorm(p, 0, 1)

# Define number of observations
m <- 40

# Sample x and generate y with f_x + random error
values_range <- round(seq(-15, 15, 0.005), 3)
x <- sort(sample(values_range, m))
sample_data <- data_frame(x = x, y = f_x(x) + error(m))

plot_sample(sample_data, values_range, q_error)

# MCMC ALGORITHM

# Fit models
GPDP_sfse_250 <- GPDPQuantReg(y ~ x, sample_data, p = 0.250)
GPDP_sfse_500 <- GPDPQuantReg(y ~ x, sample_data, p = 0.500)
GPDP_sfse_950 <- GPDPQuantReg(y ~ x, sample_data, p = 0.950)

# Save fitted models
write_rds(GPDP_sfse_250, "models/GPDP_sfse_250.rds")
write_rds(GPDP_sfse_500, "models/GPDP_sfse_500.rds")
write_rds(GPDP_sfse_950, "models/GPDP_sfse_950.rds")
#
# # Load fitted models
# GPDP_sfse_250 <- read_rds("models/GPDP_sfse_250.rds")
# GPDP_sfse_500 <- read_rds("models/GPDP_sfse_500.rds")
# GPDP_sfse_950 <- read_rds("models/GPDP_sfse_950.rds")

# Diagnose GPDP_MCMC
diagnose(GPDP_sfse_250)
diagnose(GPDP_sfse_500)
diagnose(GPDP_sfse_950)

# Predict
predictive_data <- data_frame(x = seq(-15, 15, 0.2))
credibility <- 0.90
prediction_sfse_250 <- predict(GPDP_sfse_250, predictive_data, credibility)
prediction_sfse_500 <- predict(GPDP_sfse_500, predictive_data, credibility)
prediction_sfse_950 <- predict(GPDP_sfse_950, predictive_data, credibility)

# Save prediction
write_rds(prediction_sfse_250, "predictions/prediction_sfse_250.rds")
write_rds(prediction_sfse_500, "predictions/prediction_sfse_500.rds")
write_rds(prediction_sfse_950, "predictions/prediction_sfse_950.rds")
#
# # Load prediction
# GPDP_sfse_250 <- read_rds("predictions/prediction_sfse_250.rds")
# GPDP_sfse_500 <- read_rds("predictions/prediction_sfse_500.rds")
# GPDP_sfse_950 <- read_rds("predictions/prediction_sfse_950.rds")

plot_sfse_results <- function(prediction, p) {
  return(plot_fitted_model(
    prediction = prediction,
    credibility,
    f_x,
    sample_data,
    qerror,
    p = p
  ))
}

# Plot fitted models
sfse_250_results <- plot_sfse_results(prediction_sfse_250, 0.250)
sfse_500_results <- plot_sfse_results(prediction_sfse_500, 0.500)
sfse_950_results <- plot_sfse_results(prediction_sfse_950, 0.950)
sfse_gral_results <- plot_multiple_quantiles(
  prediction_sfse_250,
  prediction_sfse_500,
  prediction_sfse_950,
  title = "Simple f & Simple error"
)

sfse_250_results
sfse_500_results
sfse_950_results
sfse_gral_results

# Save plots
ggsave(filename = "results/sfse_250_results.png", plot = sfse_250_results)
ggsave(filename = "results/sfse_500_results.png", plot = sfse_500_results)
ggsave(filename = "results/sfse_950_results.png", plot = sfse_950_results)
ggsave(filename = "results/sfse_gral_results.png", plot = sfse_gral_results)