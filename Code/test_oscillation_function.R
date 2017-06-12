library(devtools)
install_github("opardo/GPDPQuantReg")
library(GPDPQuantReg)

library(ggplot2)
library(tibble)


set.seed(19930318)

# GENERATE DATA

# Define base function
f_x <- function(x) return(x * cos(x) - sqrt(exp(x/5)) - 0.1 * (x-10)^2)

# Define error function
error <- function(m, sigma) rnorm(m, 0, sigma)

# Define number of observations
m <- 60

# Define error's variance
sigma <- 2

# Define x's range
x_range <- seq(1, 20, 0.05)

# Sample x inside the range
x <- sample(x_range, m)
x <- x[order(x)]

# Define original values (without error) inside the x's range
original_data <- data_frame(
  x = x_range,
  y = f_x(x_range)
)

# Define sample values, including error
sample_data <- data_frame(
  x = x,
  y = f_x(x) + error(m, sigma)
)

# Plot the sample data
ggplot(data = original_data, aes(x = x)) +
  geom_line(aes(y = y), color = "red") +
  geom_line(aes(y = y + qnorm(0.95, 0, sigma)), color = "blue") +
  geom_line(aes(y = y + qnorm(0.05, 0, sigma)), color = "blue") +
  geom_point(data = sample_data, aes(y = y))


# MCMC ALGORITHM

# Define parameters

# Scale data
X <- scale(as.matrix(sample_data[, -which(colnames(sample_data) == "y")]))
Y <- as.vector(scale(sample_data[["y"]]))

p <- 0.025
c_DP <- 2
d_DP <- 1
c_lambda <- 2
d_lambda <- 0.15
alpha <- sqrt(length(Y))
M <- zero_function
mcit <- 3e4
burn <- 1e4
thin <- 10

# Fit model
MCMC_output <- MCMC_GPDPQuantReg(
  X,
  Y,
  p,
  c_DP,
  d_DP,
  c_lambda,
  d_lambda,
  alpha,
  M,
  mcit,
  burn,
  thin
)

# Get scaled f's values from the model
f_data <- data.frame(matrix(
  unlist(MCMC_output$f),
  nrow=length(MCMC_output$f),
  byrow=T
))

scaled_mean <- attr(scale(sample_data$y), "scaled:center")
scaled_sigma <- attr(scale(sample_data$y), "scaled:scale")

# Calculate mean for each observation (f)
f_mean <- unname(apply(f_data, 2 ,mean)) * scaled_sigma + scaled_mean

# Calculate 0.05 percentile (f)
f_05 <- unname(apply(
  f_data,
  2 ,
  function(column) as.numeric(quantile(column, 0.025))
)) * scaled_sigma + scaled_mean

# Calculate 0.95 percentile (f)
f_95 <- unname(apply(
  f_data,
  2 ,
  function(column) as.numeric(quantile(column, 0.975))
)) * scaled_sigma + scaled_mean

# Generate comparisson data frame
comparisson <- dplyr::left_join(
  original_data,
  data_frame(
    x = sample_data$x,
    f = f_mean,
    f_025 = f_025,
    f_975 = f_975
  ),
  by = "x"
)

# Plot fitted model
ggplot(data = comparisson, aes(x = x)) +
  geom_line(aes(y = y), color = "red") +
  geom_line(aes(y = y + qnorm(0.975, 0, sigma)), color = "blue") +
  geom_line(aes(y = y + qnorm(0.025, 0, sigma)), color = "blue") +
  geom_point(aes(y = f), color = "gray") +
  geom_errorbar(aes(ymax = f_95, ymin = f_05), color = "gray") +
  geom_point(data = sample_data, aes(y = y), color = "black") +
  ggtitle(paste0("Quantile = ", p)) +
  theme(plot.title = element_text(hjust = 0.5))

# Save fitted model
#
# library(readr)
# setwd("C:/Users/PardoO/Documents/Personal/Thesis/Code")
# write_rds(MCMC_output, paste0("MCMC_QuantGPDPReg_",as.character(100*p),".rds"))
