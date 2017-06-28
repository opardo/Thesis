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
setwd(paste0(local_path,"Thesis/Applications/Simulation/complex_f_complex_error/"))

set.seed(2017)

# GENERATE DATA

# Define base function
f_x <- function(x) return(0.5 * x * cos(x) - exp(0.1 * x))

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
GPDP_cfce_250 <- GPDPQuantReg(y ~ x, sample_data, p = 0.250)
GPDP_cfce_500 <- GPDPQuantReg(y ~ x, sample_data, p = 0.500)
GPDP_cfce_950 <- GPDPQuantReg(y ~ x, sample_data, p = 0.950)

# Save fitted models
write_rds(GPDP_cfce_250, "models/GPDP_cfce_250.rds")
write_rds(GPDP_cfce_500, "models/GPDP_cfce_500.rds")
write_rds(GPDP_cfce_950, "models/GPDP_cfce_950.rds")
#
# # Load fitted models
# GPDP_cfce_250 <- read_rds("models/GPDP_cfce_250.rds")
# GPDP_cfce_500 <- read_rds("models/GPDP_cfce_500.rds")
# GPDP_cfce_950 <- read_rds("models/GPDP_cfce_950.rds")

# Diagnose GPDP_MCMC
diagnose(GPDP_cfce_250)
diagnose(GPDP_cfce_500)
diagnose(GPDP_cfce_950)

# Predict
predictive_data <- data_frame(x = seq(-15, 15, 0.2))
credibility <- 0.90
prediction_cfce_250 <- predict(GPDP_cfce_250, predictive_data, credibility)
prediction_cfce_500 <- predict(GPDP_cfce_500, predictive_data, credibility)
prediction_cfce_950 <- predict(GPDP_cfce_950, predictive_data, credibility)

# Save prediction
write_rds(prediction_cfce_250, "predictions/prediction_cfce_250.rds")
write_rds(prediction_cfce_500, "predictions/prediction_cfce_500.rds")
write_rds(prediction_cfce_950, "predictions/prediction_cfce_950.rds")
#
# # Load prediction
# GPDP_cfce_250 <- read_rds("predictions/prediction_cfce_250.rds")
# GPDP_cfce_500 <- read_rds("predictions/prediction_cfce_500.rds")
# GPDP_cfce_950 <- read_rds("predictions/prediction_cfce_950.rds")

plot_cfce_results <- function(prediction, p) {
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
cfce_250_results <- plot_cfce_results(prediction_cfce_250, 0.250)
cfce_500_results <- plot_cfce_results(prediction_cfce_500, 0.500)
cfce_950_results <- plot_cfce_results(prediction_cfce_950, 0.950)
cfce_gral_results <- plot_multiple_quantiles(
  prediction_cfce_250,
  prediction_cfce_500,
  prediction_cfce_950,
  title = "Complex f & Complex error"
)

cfce_250_results
cfce_500_results
cfce_950_results
cfce_gral_results

# Save plots
ggsave(filename = "results/cfce_250_results.png", plot = cfce_250_results)
ggsave(filename = "results/cfce_500_results.png", plot = cfce_500_results)
ggsave(filename = "results/cfce_950_results.png", plot = cfce_950_results)
ggsave(filename = "results/cfce_gral_results.png", plot = cfce_gral_results)
