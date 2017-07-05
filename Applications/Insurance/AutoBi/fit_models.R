library(insuranceData)
library(dplyr)
library(ggplot2)

data(AutoBi)

autobi <- AutoBi %>%
  filter(SEATBELT == 1 & MARITAL == 2 & CLMAGE >= 20 & CLMSEX == 2 & LOSS > 3) %>% 
  select(CLMAGE, LOSS) %>% 
  na.omit %>% 
  filter() %>% 
  rename(edad = CLMAGE, perdida = LOSS) %>% 
  mutate(edad = edad + rnorm(nrow(.), 0, 0.1))

ggplot(data = autobi, aes(edad)) +
  geom_point(aes(y = perdida))

library(devtools)
install_github("opardo/GPDPQuantReg")

library(GPDPQuantReg)
library(readr)
library(tidyr)
library(R.utils)
library(gridExtra)

# Define local_path where the repo lives (user dependent)
local_path <- "/Users/opardo/Documents/Projects/Personal/"

# Import utils
source(paste0(local_path,"Thesis/Applications/Insurance/utils.R"), local = TRUE)

# Setwd for simple function and simple error folder
setwd(paste0(local_path,"Thesis/Applications/Insurance/AutoBi"))

# Save or load data options
save <- TRUE
load <- FALSE
plots <- TRUE

set.seed(2017)

# GENERATE DATA

# MCMC ALGORITHM

# Fit models
GPDP_25 <- GPDPQuantReg(perdida ~ edad, autobi, p = 0.25, M = zero_function, d_DP = 2, d_lambda = 2, burn = 5e3, mcit = 1e4, thin = 5)
GPDP_50 <- GPDPQuantReg(perdida ~ edad, autobi, p = 0.50, M = zero_function, d_DP = 2, d_lambda = 2, burn = 5e3, mcit = 1e4, thin = 5)
GPDP_95 <- GPDPQuantReg(perdida ~ edad, autobi, p = 0.95, M = zero_function, d_DP = 2, d_lambda = 2, burn = 5e3, mcit = 1e4, thin = 5)

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
predictive_data <- data_frame(x = seq(20, 85, 1))
credibility <- 0.90
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

mean(unlist(GPDP_25$parameters$alternative))
mean(unlist(GPDP_50$parameters$alternative))
mean(unlist(GPDP_95$parameters$alternative))

