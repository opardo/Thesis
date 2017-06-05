library(readr)
library(ggplot2)
library(tibble)

setwd("C:/Users/PardoO/Documents/Personal/Thesis/Code")

set.seed(19930318)

################################################################################
################################ RECREATE DATA #################################
################################################################################

# Define base function
f_x <- function(x) return(10*sin(x/2) +  x - 2 * log(1/x))

# Define error function
error <- function(m, sigma) rnorm(m, 0, sigma)

# Define number of observations
m <- 45

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

################################################################################
############################## PLOT FITTED MODELS ##############################
################################################################################

################################ QUANTILE = 0.5 ################################

# Load model's data
MCMC_output <- read_rds("MCMC_QuantGPDPReg_50.rds")

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
  function(column) as.numeric(quantile(column, 0.05))
)) * scaled_sigma + scaled_mean

# Calculate 0.95 percentile (f)
f_95 <- unname(apply(
  f_data,
  2 ,
  function(column) as.numeric(quantile(column, 0.95))
)) * scaled_sigma + scaled_mean

# Generate comparisson data frame
comparisson <- dplyr::left_join(
  original_data,
  data_frame(
    x = sample_data$x,
    f = f_mean,
    f_05 = f_05,
    f_95 = f_95
  ),
  by = "x"
)

# Plot fitted model
ggplot(data = comparisson, aes(x = x)) +
  geom_line(aes(y = y), color = "red") +
  geom_line(aes(y = y + qnorm(0.95, 0, sigma)), color = "blue") +
  geom_line(aes(y = y + qnorm(0.05, 0, sigma)), color = "blue") +
  geom_point(aes(y = f), color = "gray") +
  geom_errorbar(aes(ymax = f_95, ymin = f_05), color = "gray") +
  geom_point(data = sample_data, aes(y = y), color = "black") +
  ggtitle("Quantile = 0.50") +
  theme(plot.title = element_text(hjust = 0.5))

############################### QUANTILE = 0.05 ################################

# Load model's data
MCMC_output <- read_rds("MCMC_QuantGPDPReg_05.rds")

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
  function(column) as.numeric(quantile(column, 0.05))
)) * scaled_sigma + scaled_mean

# Calculate 0.95 percentile (f)
f_95 <- unname(apply(
  f_data,
  2 ,
  function(column) as.numeric(quantile(column, 0.95))
)) * scaled_sigma + scaled_mean

# Generate comparisson data frame
comparisson <- dplyr::left_join(
  original_data,
  data_frame(
    x = sample_data$x,
    f = f_mean,
    f_05 = f_05,
    f_95 = f_95
  ),
  by = "x"
)

# Plot fitted model
ggplot(data = comparisson, aes(x = x)) +
  geom_line(aes(y = y), color = "red") +
  geom_line(aes(y = y + qnorm(0.95, 0, sigma)), color = "blue") +
  geom_line(aes(y = y + qnorm(0.05, 0, sigma)), color = "blue") +
  geom_point(aes(y = f), color = "gray") +
  geom_errorbar(aes(ymax = f_95, ymin = f_05), color = "gray") +
  geom_point(data = sample_data, aes(y = y), color = "black") +
  ggtitle("Quantile = 0.05") +
  theme(plot.title = element_text(hjust = 0.5))

############################### QUANTILE = 0.95 ################################

# Load model's data
MCMC_output <- read_rds("MCMC_QuantGPDPReg_95.rds")

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
  function(column) as.numeric(quantile(column, 0.05))
)) * scaled_sigma + scaled_mean

# Calculate 0.95 percentile (f)
f_95 <- unname(apply(
  f_data,
  2 ,
  function(column) as.numeric(quantile(column, 0.95))
)) * scaled_sigma + scaled_mean

# Generate comparisson data frame
comparisson <- dplyr::left_join(
  original_data,
  data_frame(
    x = sample_data$x,
    f = f_mean,
    f_05 = f_05,
    f_95 = f_95
  ),
  by = "x"
)

# Plot fitted model
ggplot(data = comparisson, aes(x = x)) +
  geom_line(aes(y = y), color = "red") +
  geom_line(aes(y = y + qnorm(0.95, 0, sigma)), color = "blue") +
  geom_line(aes(y = y + qnorm(0.05, 0, sigma)), color = "blue") +
  geom_point(aes(y = f), color = "gray") +
  geom_errorbar(aes(ymax = f_95, ymin = f_05), color = "gray") +
  geom_point(data = sample_data, aes(y = y), color = "black") +
  ggtitle("Quantile = 0.95") +
  theme(plot.title = element_text(hjust = 0.5))
