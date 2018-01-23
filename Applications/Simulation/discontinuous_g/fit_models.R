library(devtools)
install_github("opardo/GPDPQuantReg")

library(GPDPQuantReg)
library(ggplot2)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(R.utils)
library(gridExtra)

# Define local_path where the repo lives (user dependent)
local_path <- "/Users/opardo/Documents/Projects/Personal/"

# Import utils
source(paste0(local_path,"Thesis/Applications/Simulation/utils.R"), local = TRUE)

# Setwd for simple function and simple error folder
setwd(paste0(local_path,"Thesis/Applications/Simulation/discontinuous_g/"))

# Save or load data options
save <- TRUE
load <- FALSE
plots <- TRUE

set.seed(2018)

# GENERATE DATA

# Define base function
g_x <- function(x) {
  return(ifelse(
    x < - 5,
    - x / 5 + 2 * cos(x) - 2,
    ifelse(
      x < 5,
      0.1 * x ** 2 - 0.5 * x + 1,
      6 - log(x)
    )
  ))
}
  
# Define error function and inverse error function
error <- function(x) rnorm(length(x), 0, 1)
qerror <- function(p) qnorm(p, 0, 1)

# Define number of observations
m <- 60

# Sample x and generate y with g_x + random error
values_range <- round(seq(-15, 15, 0.01), 3)
x <- sort(sample(values_range, m))
sample_data <- data_frame(x = x, y = g_x(x) + error(x))

sample_plot <- plot_sample(sample_data, values_range, qerror, g_x)
sample_plot

# MCMC ALGORITHM

# Fit models
GPDP_25 <- GPDPQuantReg(y ~ x, sample_data, p = 0.25, d_DP = 2, d_lambda = 2, mcit = 15000, burn = 5000, thin = 5)
GPDP_50 <- GPDPQuantReg(y ~ x, sample_data, p = 0.50, d_DP = 2, d_lambda = 2, mcit = 15000, burn = 5000, thin = 5)
GPDP_95 <- GPDPQuantReg(y ~ x, sample_data, p = 0.95, d_DP = 2, d_lambda = 2, mcit = 15000, burn = 5000, thin = 5)

# Save/load fitted models
if (save) {
  write_rds(GPDP_25, "models/GPDP_25.rds")
  write_rds(GPDP_50, "models/GPDP_50.rds")
  write_rds(GPDP_95, "models/GPDP_95.rds")
}
if (load) {
  GPDP_25 <- read_rds("models/GPDP_25.rds")
  GPDP_50 <- read_rds("models/GPDP_50.rds")
  GPDP_95 <- read_rds("models/GPDP_95.rds")
}

# Diagnose GPDP_MCMC
diagnose(GPDP_25)
diagnose(GPDP_50)
diagnose(GPDP_95)

# Predict
predictive_data <- data_frame(x = seq(-15, 15, 0.1))
credibility <- 0.95
prediction_25 <- predict(GPDP_25, predictive_data, credibility)
prediction_50 <- predict(GPDP_50, predictive_data, credibility)
prediction_95 <- predict(GPDP_95, predictive_data, credibility)

# Save/load prediction
if (save) {
  write_rds(prediction_25, "predictions/prediction_25.rds")
  write_rds(prediction_50, "predictions/prediction_50.rds")
  write_rds(prediction_95, "predictions/prediction_95.rds")
}
if (load) {
  prediction_25 <- read_rds("predictions/prediction_25.rds")
  prediction_50 <- read_rds("predictions/prediction_50.rds")
  prediction_95 <- read_rds("predictions/prediction_95.rds")
}

# Get plots' limits
upper_limit <- max(prediction_95$upper)
lower_limit <- min(prediction_25$lower)

plot_results <- function(prediction, p) {
  return(plot_fitted_model(
    prediction = prediction,
    credibility,
    g_x,
    sample_data,
    qerror,
    p = p,
    lower_limit,
    upper_limit
  ))
}

# Plot fitted models
results_25 <- plot_results(prediction_25, 0.25)
results_50 <- plot_results(prediction_50, 0.50)
results_95 <- plot_results(prediction_95, 0.95)

fitted_models <- grid.arrange(
  results_25,
  results_50,
  results_95,
  ncol = 3
)

results_25
results_50
results_95

# Save plots
if (plots) {
  ggsave(filename = "results/sample.png", plot = sample_plot)
  ggsave(
    filename = "results/fitted_models.png",
    plot = fitted_models,
    width = 12,
    height = 5
  )
}