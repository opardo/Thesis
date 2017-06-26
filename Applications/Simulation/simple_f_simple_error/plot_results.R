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
  "Thesis/Applications/Simulation/simple_f_simple_error/fitted_models/"
))

# Load fitted models
GPDP_sfse_250 <- read_rds("GPDP_sfse_250.rds")
GPDP_sfse_500 <- read_rds("GPDP_sfse_500.rds")
GPDP_sfse_950 <- read_rds("GPDP_sfse_950.rds")

# Get GPDP diagnostics
diagnostic_GPDP(GPDP_sfse_250)
diagnostic_GPDP(GPDP_sfse_500)
diagnostic_GPDP(GPDP_sfse_950)

# Predict
Xp <- as.matrix(seq(-15, 15, 0.1))
credibility <- 0.90
sfse_250_prediction <- predict_GPDP(GPDP_sfse_250, Xp, credibility)
sfse_500_prediction <- predict_GPDP(GPDP_sfse_500, Xp, credibility)
sfse_950_prediction <- predict_GPDP(GPDP_sfse_950, Xp, credibility)

# Recreate simulated data
set.seed(2017)

f_x <- function(x) return((1/40) * x^2 - (1/20) * x - 2)
error <- function(m, params) rnorm(m, 0, params[1])
qerror <- function(p, params) qnorm(p, 0, params[1])
m <- 40
params <- c(1)
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

plot_sfse_results <- function(prediction, p) {
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
sfse_250_results <- plot_sfse_results(sfse_250_prediction, 0.250)
sfse_500_results <- plot_sfse_results(sfse_500_prediction, 0.500)
sfse_950_results <- plot_sfse_results(sfse_950_prediction, 0.950)

sfse_250_results
sfse_500_results
sfse_950_results

# Save results

setwd(paste0(
  local_path,
  "Thesis/Applications/Simulation/simple_f_simple_error/results/"
))

ggsave(filename="sfse_250_results.png", plot=sfse_250_results)
ggsave(filename="sfse_500_results.png", plot=sfse_500_results)
ggsave(filename="sfse_950_results.png", plot=sfse_950_results)
