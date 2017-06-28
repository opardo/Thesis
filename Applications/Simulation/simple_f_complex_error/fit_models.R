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
setwd(paste0(local_path,"Thesis/Applications/Simulation/simple_f_complex_error/"))

set.seed(2017)

# GENERATE DATA

# Define base function
f_x <- function(x) return(0.5 * x)

# Define error function and inverse error function
error <- function(m) rgamma(m, 2, 1)
qerror <- function(p) qgamma(p, 2, 1)

# Define number of observations
m <- 40

# Sample x and generate y with f_x + random error
values_range <- round(seq(-15, 15, 0.005), 3)
x <- sort(sample(values_range, m))
sample_data <- data_frame(x = x, y = f_x(x) + error(m))

plot_sample(sample_data, values_range, q_error)

# MCMC ALGORITHM

# Fit models
GPDP_sfce_250 <- GPDPQuantReg(y ~ x, sample_data, p = 0.250)
GPDP_sfce_500 <- GPDPQuantReg(y ~ x, sample_data, p = 0.500)
GPDP_sfce_950 <- GPDPQuantReg(y ~ x, sample_data, p = 0.950)

# Save fitted models
write_rds(GPDP_sfce_250, "models/GPDP_sfce_250.rds")
write_rds(GPDP_sfce_500, "models/GPDP_sfce_500.rds")
write_rds(GPDP_sfce_950, "models/GPDP_sfce_950.rds")
#
# # Load fitted models
# GPDP_sfce_250 <- read_rds("models/GPDP_sfce_250.rds")
# GPDP_sfce_500 <- read_rds("models/GPDP_sfce_500.rds")
# GPDP_sfce_950 <- read_rds("models/GPDP_sfce_950.rds")

# Diagnose GPDP_MCMC
diagnose(GPDP_sfce_250)
diagnose(GPDP_sfce_500)
diagnose(GPDP_sfce_950)

# Predict
predictive_data <- data_frame(x = seq(-15, 15, 0.2))
credibility <- 0.90
prediction_sfce_250 <- predict(GPDP_sfce_250, predictive_data, credibility)
prediction_sfce_500 <- predict(GPDP_sfce_500, predictive_data, credibility)
prediction_sfce_950 <- predict(GPDP_sfce_950, predictive_data, credibility)

# Save prediction
write_rds(prediction_sfce_250, "predictions/prediction_sfce_250.rds")
write_rds(prediction_sfce_500, "predictions/prediction_sfce_500.rds")
write_rds(prediction_sfce_950, "predictions/prediction_sfce_950.rds")
#
# # Load prediction
# GPDP_sfce_250 <- read_rds("predictions/prediction_sfce_250.rds")
# GPDP_sfce_500 <- read_rds("predictions/prediction_sfce_500.rds")
# GPDP_sfce_950 <- read_rds("predictions/prediction_sfce_950.rds")

plot_sfce_results <- function(prediction, p) {
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
sfce_250_results <- plot_sfce_results(prediction_sfce_250, 0.250)
sfce_500_results <- plot_sfce_results(prediction_sfce_500, 0.500)
sfce_950_results <- plot_sfce_results(prediction_sfce_950, 0.950)
sfce_gral_results <- plot_multiple_quantiles(
  prediction_sfce_250,
  prediction_sfce_500,
  prediction_sfce_950,
  title = "Simple f & Complex error"
)

sfce_250_results
sfce_500_results
sfce_950_results
sfce_gral_results

# Save plots
ggsave(filename = "results/sfce_250_results.png", plot = sfce_250_results)
ggsave(filename = "results/sfce_500_results.png", plot = sfce_500_results)
ggsave(filename = "results/sfce_950_results.png", plot = sfce_950_results)
ggsave(filename = "results/sfce_gral_results.png", plot = sfce_gral_results)