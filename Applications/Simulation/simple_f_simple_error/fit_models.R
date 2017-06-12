library(devtools)
install_github("opardo/GPDPQuantReg", force = TRUE)

library(GPDPQuantReg)
library(ggplot2)
library(tibble)

set.seed(2017)

# GENERATE DATA

# Define base function
f_x <- function(x) return((1/40) * x^2 - (1/20) * x - 2)

# Define error function
error <- function(m, sigma) rnorm(m, 0, sigma)

# Define inverse error function
qerror <- function(p, sigma) qnorm(p, 0, sigma)

# Define number of observations
m <- 40

# Define error's variance
sigma <- 1

# Define x's range
x_range <- seq(-15, 15, 0.005)

# Sample x inside the range
x <- sample(x_range, m)
x <- x[order(x)]

# Define original values (without error) inside the x's range
original_data <- data_frame(
  x = x_range,
  f = f_x(x_range)
)

# Define sample values, including error
sample_data <- data_frame(
  x = x,
  y = f_x(x) + error(m, sigma)
)

# Join datasets to plot
simulated_dataset <- dplyr::left_join(
  original_data,
  sample_data,
  by = "x"
)

# Plot the simulated data
ggplot(data = simulated_dataset, aes(x = x)) +
  geom_line(aes(y = f), color = "red") +
  geom_line(aes(y = f + qerror(0.975, sigma)), color = "blue") +
  geom_line(aes(y = f + qerror(0.025, sigma)), color = "blue") +
  geom_point(aes(y = y))

# MCMC ALGORITHM

# Define parameters
X <- as.matrix(sample_data[, -which(colnames(sample_data) == "y")])
Y <- as.vector(sample_data[["y"]])

# Fit models
MCMC_sfse_250 <- MCMC_GPDPQuantReg(X, Y, p = 0.250)
MCMC_sfse_500 <- MCMC_GPDPQuantReg(X, Y, p = 0.500)
MCMC_sfse_950 <- MCMC_GPDPQuantReg(X, Y, p = 0.950)

# # Save fitted models
# library(readr)
# 
# local_path <- "C:/Users/PardoO/Documents/Personal/"
# setwd(paste0(
#   local_path,
#   "Thesis/Applications/Simulation/simple_f_simple_error/fitted_models/"
# ))
# 
# write_rds(MCMC_sfse_250, "MCMC_sfse_250.rds")
# write_rds(MCMC_sfse_500, "MCMC_sfse_500.rds")
# write_rds(MCMC_sfse_950, "MCMC_sfse_950.rds")
