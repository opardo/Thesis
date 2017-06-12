library(devtools)
install_github("opardo/GPDPQuantReg")

library(GPDPQuantReg)
library(ggplot2)
library(tibble)

set.seed(2017)

# GENERATE DATA

# Define base function
f_x <- function(x) return(0.5 * x * cos(x) - exp(0.1 * x))

# Define error function
error <- function(m, alpha_error, beta_error) rgamma(m, alpha_error, beta_error)

# Define inverse error function
qerror <- function(p, alpha_error, beta_error) qgamma(p, alpha_error, beta_error)

# Define number of observations
m <- 40

# Define error's parameters
alpha_error <- 1
beta_error <- 1

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
  y = f_x(x) + error(m, alpha_error, beta_error)
)

# Join datasets to plot
simulated_dataset <- dplyr::left_join(
  original_data,
  sample_data,
  by = "x"
)

# Plot the simulated data
ggplot(data = simulated_dataset, aes(x = x)) +
  geom_line(aes(y = f + qerror(0.500, alpha_error, beta_error)), color = "red") +
  geom_line(aes(y = f + qerror(0.975, alpha_error, beta_error)), color = "blue") +
  geom_line(aes(y = f + qerror(0.025, alpha_error, beta_error)), color = "blue") +
  geom_point(aes(y = y))

# MCMC ALGORITHM

# Define parameters
X <- as.matrix(sample_data[, -which(colnames(sample_data) == "y")])
Y <- as.vector(sample_data[["y"]])

# Fit models
MCMC_cfce_250 <- MCMC_GPDPQuantReg(X, Y, p = 0.250)
MCMC_cfce_500 <- MCMC_GPDPQuantReg(X, Y, p = 0.500)
MCMC_cfce_950 <- MCMC_GPDPQuantReg(X, Y, p = 0.950)

# Save fitted models
library(readr)

setwd(paste0(
  "/Users/opardo/Documents/Projects/Personal/",
  "Thesis/Applications/Simulation/complex_f_complex_error/fitted_models/"
))

write_rds(MCMC_cfce_250, "MCMC_cfce_250.rds")
write_rds(MCMC_cfce_500, "MCMC_cfce_500.rds")
write_rds(MCMC_cfce_950, "MCMC_cfce_950.rds")
