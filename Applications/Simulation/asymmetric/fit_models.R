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
library(xtable)

# Define local_path where the repo lives (user dependent)
local_path <- "/Users/opardo/Documents/Projects/Personal/"

# Import utils
source(paste0(local_path,"Thesis/Applications/Simulation/utils.R"), local = TRUE)
source(paste0(local_path,"Thesis/Applications/trad_quant_reg.R"), local = TRUE)

# Setwd for simple function and simple error folder
setwd(paste0(local_path,"Thesis/Applications/Simulation/asymmetric/"))

# Save or load data options
run <- FALSE
save <- FALSE
load <- TRUE
plots <- FALSE

set.seed(20183)

# GENERATE DATA

# Define base function
g_x <- function(x) return(0.2 * x * cos(x) - 0.2 * exp(0.1 * x))
  
# Define error function and inverse error function
error <- function(x) rgamma(length(x), 1, 1)
qerror <- function(p, x) qgamma(p, 1, 1)

# Define number of observations
m <- 60

# Sample x and generate y with g_x + random error
values_range <- round(seq(-15, 15, 0.01), 3)
x <- sort(sample(values_range, m))
sample_data <- data_frame(x = x, y = g_x(x) + error(x))
sample_trad_data <- sample_data %>% 
  mutate(x1 = x, x2 = x^2, x3 = x^3, x4 = x^4, x5 = x^5) %>% 
  select(x1, x2, x3, x4, x5, y)

sample_plot <- plot_sample(sample_data, values_range, qerror, g_x, -5, 10)
sample_plot

# MCMC ALGORITHM

# Fit models
ptm <- proc.time()
TradReg_params <- TradQuantReg(sample_trad_data, y ~ .)
time_trad_fit <- proc.time() - ptm

# Save/load fitted models
if (run) {
  ptm <- proc.time()
  GPDP_25 <- GPDPQuantReg(y ~ x, sample_data, p = 0.25, d_DP = 2, d_lambda = 2, mcit = 15000, burn = 5000, thin = 5)
  GPDP_50 <- GPDPQuantReg(y ~ x, sample_data, p = 0.50, d_DP = 2, d_lambda = 2, mcit = 15000, burn = 5000, thin = 5)
  GPDP_95 <- GPDPQuantReg(y ~ x, sample_data, p = 0.95, d_DP = 2, d_lambda = 2, mcit = 15000, burn = 5000, thin = 5)
  time_fit <- proc.time() - ptm
  
  if (save) {
    write_rds(GPDP_25, "models/GPDP_25.rds")
    write_rds(GPDP_50, "models/GPDP_50.rds")
    write_rds(GPDP_95, "models/GPDP_95.rds")
    write_rds(time_fit, "models/time_fit.rds")
  }
}
if (load) {
  GPDP_25 <- read_rds("models/GPDP_25.rds")
  GPDP_50 <- read_rds("models/GPDP_50.rds")
  GPDP_95 <- read_rds("models/GPDP_95.rds")
  time_fit <- read_rds("models/time_fit.rds")
}

# Diagnose GPDP_MCMC
diagnose(GPDP_25)
diagnose(GPDP_50)
diagnose(GPDP_95)

# Predict
predictive_data <- data_frame(x = seq(-15, 15, 0.1))
predictive_trad_data <- predictive_data %>% 
  mutate(x1 = x, x2 = x^2, x3 = x^3, x4 = x^4, x5 = x^5) %>% 
  select(x1, x2, x3, x4, x5)
credibility <- 0.95

ptm <- proc.time()
trad_predictions <- TradQuantReg_predict(
  TradReg_params,
  predictive_trad_data,
  c(0.25, 0.5, 0.95),
  credibility
)
trad_prediction_25 <- trad_predictions[['0.25']] 
trad_prediction_50 <- trad_predictions[['0.5']] 
trad_prediction_95 <- trad_predictions[['0.95']]
time_trad_pred <- proc.time() - ptm

# Save/load prediction
if (run) {
  ptm <- proc.time()
  prediction_25 <- predict(GPDP_25, predictive_data, credibility)
  prediction_50 <- predict(GPDP_50, predictive_data, credibility)
  prediction_95 <- predict(GPDP_95, predictive_data, credibility)
  time_pred <- proc.time() - ptm
  
  if (save) {
    write_rds(prediction_25, "predictions/prediction_25.rds")
    write_rds(prediction_50, "predictions/prediction_50.rds")
    write_rds(prediction_95, "predictions/prediction_95.rds")
    write_rds(time_pred, "predictions/time_pred.rds")
  }
}
if (load) {
  prediction_25 <- read_rds("predictions/prediction_25.rds")
  prediction_50 <- read_rds("predictions/prediction_50.rds")
  prediction_95 <- read_rds("predictions/prediction_95.rds")
  time_pred <- read_rds("predictions/time_pred.rds")
}

plot_results <- function(prediction, p, title = "Modelo GPDP") {
  return(plot_fitted_model(
    prediction = prediction,
    credibility,
    g_x,
    sample_data,
    qerror,
    p = p,
    lower_limit = -7,
    upper_limit = 13,
    title
  ))
}

# Plot fitted models
trad_results_25 <- plot_results(trad_prediction_25, 0.25, "Modelo Tradicional") 
trad_results_50 <- plot_results(trad_prediction_50, 0.50, "Modelo Tradicional")
trad_results_95 <- plot_results(trad_prediction_95, 0.95, "Modelo Tradicional")

results_25 <- plot_results(prediction_25, 0.25)
results_50 <- plot_results(prediction_50, 0.50)
results_95 <- plot_results(prediction_95, 0.95)

predictions_plot <- grid.arrange(
  trad_results_95,
  results_95,
  trad_results_50,
  results_50,
  trad_results_25,
  results_25,
  ncol = 2
)

# Save plots
if (plots) {
  ggsave(
    filename = "results/sample.png",
    plot = sample_plot,
    width = 7,
    height = 5
  )
  ggsave(
    filename = "results/predictions.png",
    plot = predictions_plot ,
    width = 9,
    height = 11
  )
}

# Comparison
comp_matrix_metric <- function(func){
  return(gral_comp_matrix_metric(
    trad_prediction_95, trad_prediction_50, trad_prediction_25, 
    prediction_95, prediction_50, prediction_25,
    func
  ))
}

# MSE between prediction and real value
mse <- comp_matrix_metric(mse_pred) 
# Squared-correlation between prediction and real value
corr <- comp_matrix_metric(cor_pred)
# Real values' percentage within the credibility interval
withn <- comp_matrix_metric(within_pred) 

xtable(
  mse,
  caption = "Error cuadrático promedio (datos con error asimétrico)",
  align = c("c","c","c"),
  label = "mse_asymmetric"
)
xtable(
  corr,
  caption = "Correlación al cuadrado (datos con error asimétrico)",
  align = c("c","c","c"),
  label = "corr_asymmetric"
)
xtable(
  100 * withn,
  caption = "Porcentaje de valores reales dentro del intervalo de confianza (datos con error asimétrico)",
  align = c("c","c","c"),
  digits = c(2,0,0),
  label = "within_asymmetric"
)
