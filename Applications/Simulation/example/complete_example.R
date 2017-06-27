library(devtools)
install_github("opardo/GPDPQuantReg")

library(GPDPQuantReg)
library(ggplot2)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(R.utils)

ptm <- proc.time()

# Define local_path where the repo lives (user dependent)
local_path <- "/Users/opardo/Documents/Projects/Personal/"

# Import utils
source(paste0(local_path,"Thesis/Applications/Simulation/utils.R"), local = TRUE)

# Setwd for simple function and simple error folder
setwd(paste0(local_path,"Thesis/Applications/Simulation/example/"))

set.seed(201707)

# GENERATE DATA

# Define base function
f_x <- function(x) return(0.5 * x * cos(x) - exp(0.1 * x))

# Define error function and inverse error function
error <- function(m) rgamma(m, 2, 1)
qerror <- function(p) qgamma(p, 2, 1)

# Define number of observations
m <- 20

# Sample x and generate y with f_x + random error
x <- sort(sample(seq(-15, 15, 0.005), m))
sample_data <- data.frame(x = x, y = f_x(x) + error(m))

sample_data$x <- round(sample_data$x, 3)
plot_sample(sample_data, round(seq(-15, 15, 0.005), 3), q_error)

# MCMC ALGORITHM

# Fit models
GPDP_MCMC <- GPDPQuantReg(y ~ x, sample_data, p = 0.250)

# Save fitted models
write_rds(GPDP_MCMC, "models/GPDP_example_250.rds")

# # Load fitted models
# GPDP_MCMC <- read_rds("models/GPDP_example_250.rds")

# Diagnose GPDP_MCMC
diagnose(GPDP_MCMC)

# Predict
predictive_data <- data_frame(x = seq(-15, 15, 0.2))
credibility <- 0.90
prediction <- predict(GPDP_MCMC, predictive_data, credibility)

# Save prediction
write_rds(prediction, "predictions/prediction_example_250.rds")

# # Load prediction
# prediction <- read_rds("predictions/prediction_example_250.rds")

plot_example_results <- function(prediction, p) {
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
example_250_results <- plot_example_results(prediction, 0.250)

example_250_results

# Save plots
ggsave(filename = "results/example_250_results.png", plot = example_250_results)

proc.time() - ptm
