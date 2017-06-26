library(devtools)
install_github("opardo/GPDPQuantReg")

library(ggplot2)
library(tibble)
library(readr)
library(dplyr)
library(R.utils)
library(GPDPQuantReg)

local_path <- "/Users/opardo/Documents/Projects/Personal/"

setwd(paste0(
  local_path,
  "Thesis/Applications/Simulation/"
))

function_files <- sourceDirectory("utils")
for (file in function_files){
  source(file, local = TRUE)
}

setwd(paste0(
  local_path,
  "Thesis/Applications/Simulation/complex_f_complex_error/fitted_models/"
))

# Load fitted models
GPDP_cfce_250 <- read_rds("GPDP_cfce_250.rds")
GPDP_cfce_500 <- read_rds("GPDP_cfce_500.rds")
GPDP_cfce_950 <- read_rds("GPDP_cfce_950.rds")

# Get GPDP diagnostics
diagnostic_GPDP(GPDP_cfce_250)
diagnostic_GPDP(GPDP_cfce_500)
diagnostic_GPDP(GPDP_cfce_950)

# Predict
Xp <- as.matrix(seq(-15, 15, 0.1))
credibility <- 0.90
cfce_250_prediction <- predict_GPDP(GPDP_cfce_250, Xp, credibility)
cfce_500_prediction <- predict_GPDP(GPDP_cfce_500, Xp, credibility)
cfce_950_prediction <- predict_GPDP(GPDP_cfce_950, Xp, credibility)

# Recreate simulated data
set.seed(2017)

f_x <- function(x) return(0.5 * x * cos(x) - exp(0.1 * x))
error <- function(m, params) rgamma(m, params[1], params[2])
qerror <- function(p, params) qgamma(p, params[1], params[2])
m <- 40
params <- c(1, 1)
x_range <- seq(-15, 15, 0.005)
x <- sample(x_range, m)
x <- x[order(x)]

original_data <- data_frame(
  x = round(x_range, 3),
  f = f_x(x_range)
)

sample_data <- data_frame(
  x = x,
  y = f_x(x) + error(m, params)
)

plot_cfce_results <- function(prediction, p) {
  return(plot_fitted_model(
    prediction = prediction,
    credibility,
    original_data,
    sample_data,
    qerror, 
    params, 
    p = p
  ))
}

# Plot fitted models
cfce_250_results <- plot_cfce_results(cfce_250_prediction, 0.250)
cfce_500_results <- plot_cfce_results(cfce_500_prediction, 0.500)
cfce_950_results <- plot_cfce_results(cfce_950_prediction, 0.950)

cfce_250_results
cfce_500_results
cfce_950_results

# Save results

setwd(paste0(
  local_path,
  "Thesis/Applications/Simulation/complex_f_complex_error/results/"
))

ggsave(filename="cfce_250_results.png", plot=cfce_250_results)
ggsave(filename="cfce_500_results.png", plot=cfce_500_results)
ggsave(filename="cfce_950_results.png", plot=cfce_950_results)