library(devtools)
install_github("opardo/GPDPQuantReg")

library(ggplot2)
library(tibble)
library(readr)
library(GPDPQuantReg)

# Recreate simulated data
set.seed(2017)

f_x <- function(x) return(0.5 * x * cos(x) - exp(0.1 * x))
error <- function(m, sigma) rnorm(m, 0, sigma)
qerror <- function(p, sigma) qnorm(p, 0, sigma)
m <- 40
sigma <- 1
x_range <- seq(-15, 15, 0.005)
x <- sample(x_range, m)
x <- x[order(x)]

original_data <- data_frame(
  x = x_range,
  f = f_x(x_range)
)

sample_data <- data_frame(
  x = x,
  y = f_x(x) + error(m, sigma)
)

plot_fitted_model <- function(MCMC_output, p) {

  # Get f's values from the model
  f_data <- data.frame(matrix(
    unlist(MCMC_output$f),
    nrow=length(MCMC_output$f),
    byrow=T
  ))

  # Calculate mean for each observation, and its confidence interval (f)
  f_mean <- unname(apply(f_data, 2, mean))
  f_025 <- unname(apply(f_data, 2, function(column) as.numeric(quantile(column, 0.025))))
  f_975 <- unname(apply(f_data, 2, function(column) as.numeric(quantile(column, 0.975))))

  # Generate comparisson data frame
  comparisson <- dplyr::left_join(
    original_data,
    data_frame(
      x = sample_data$x,
      f_mean = f_mean,
      f_025 = f_025,
      f_975 = f_975
    ),
    by = "x"
  )

  # Get plot
  fitted_model_plot <- ggplot(data = comparisson, aes(x = x)) +
    geom_line(aes(y = f + qerror(p, sigma)), color = "blue") +
    geom_point(aes(y = f_mean), color = "gray") +
    geom_errorbar(aes(ymax = f_975, ymin = f_025), color = "gray") +
    geom_point(data = sample_data, aes(y = y), color = "black") +
    ggtitle(paste0("Quantile = ", p)) +
    theme(plot.title = element_text(hjust = 0.5))

  return(fitted_model_plot)
}

setwd(paste0(
  "C:/Users/PardoO/Documents/Personal/",
  "Thesis/Applications/Simulation/complex_f_simple_error/fitted_models/"
))

# Load fitted models
MCMC_cfse_250 <- read_rds("MCMC_cfse_250.rds")
MCMC_cfse_500 <- read_rds("MCMC_cfse_500.rds")
MCMC_cfse_950 <- read_rds("MCMC_cfse_950.rds")

# Get MCMC diagnostics
gpdp_mcmc_diagnostics(MCMC_cfse_250)
gpdp_mcmc_diagnostics(MCMC_cfse_500)
gpdp_mcmc_diagnostics(MCMC_cfse_950)

# Plot fitted models
cfse_250_results <- plot_fitted_model(MCMC_cfse_250, 0.250)
cfse_500_results <- plot_fitted_model(MCMC_cfse_500, 0.500)
cfse_950_results <- plot_fitted_model(MCMC_cfse_950, 0.950)

cfse_250_results
cfse_500_results
cfse_950_results

# # Save results
# 
# local_path <- "C:/Users/PardoO/Documents/Personal/"
# setwd(paste0(
#   local_path,
#   "Thesis/Applications/Simulation/complex_f_simple_error/results/"
# ))
# 
# ggsave(filename="cfse_250_results.png", plot=cfse_250_results)
# ggsave(filename="cfse_500_results.png", plot=cfse_500_results)
# ggsave(filename="cfse_950_results.png", plot=cfse_950_results)
