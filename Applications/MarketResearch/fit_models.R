library(devtools)
install_github("opardo/GPDPQuantReg")

library(GPDPQuantReg)

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Define local_path where the repo lives (user dependent)
local_path <- "C:/Users/MB75168/Documents/Projects/Personal/"

# Setwd for simple function and simple error folder
setwd(paste0(local_path,"Thesis/Applications/MarketResearch/"))

#### LOAD DATA ####

brand_awareness_investment <- read_csv("brand_awareness_investment.csv") %>%
  mutate(weeks = dmy(weeks))

train_bai <- brand_awareness_investment %>%
  filter(year(weeks) < 2016) %>%
  select(-weeks)

test_bai <- brand_awareness_investment %>%
  filter(year(weeks) == 2016)

#### FIT MODELS ####

set.seed(2017)

GPDP_500 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.500, d_DP = 2, d_lambda = 2, mcit = 1e4, burn = 5e3, thin = 5)
GPDP_050 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.050, d_DP = 2, d_lambda = 2, mcit = 1e4, burn = 5e3, thin = 5)
GPDP_250 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.250, d_DP = 2, d_lambda = 2, mcit = 1e4, burn = 5e3, thin = 5)
GPDP_750 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.750, d_DP = 2, d_lambda = 2, mcit = 1e4, burn = 5e3, thin = 5)
GPDP_950 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.950, d_DP = 2, d_lambda = 2, mcit = 1e4, burn = 5e3, thin = 5)

# Save / load models

# write_rds(GPDP_500, "models/GPDP_500.rds")
# write_rds(GPDP_050, "models/GPDP_050.rds")
# write_rds(GPDP_250, "models/GPDP_250.rds")
# write_rds(GPDP_750, "models/GPDP_750.rds")
# write_rds(GPDP_950, "models/GPDP_950.rds")
GPDP_500 <- read_rds("models/GPDP_500.rds")
GPDP_050 <- read_rds("models/GPDP_050.rds")
GPDP_250 <- read_rds("models/GPDP_250.rds")
GPDP_750 <- read_rds("models/GPDP_750.rds")
GPDP_950 <- read_rds("models/GPDP_950.rds")

##### FIT REVIEW ####

# Diagnose GPDP_MCMC
diagnose(GPDP_500)
diagnose(GPDP_050)
diagnose(GPDP_250)
diagnose(GPDP_750)
diagnose(GPDP_950)

# Predict test data with model
credibility <- 0.90
train_500 <- predict(GPDP_500, train_bai, credibility)
train_050 <- predict(GPDP_050, train_bai, credibility)
train_950 <- predict(GPDP_950, train_bai, credibility)

# Save/load prediction

# write_rds(train_500, "predictions/train_500.rds")
# write_rds(train_050, "predictions/train_050.rds")
# write_rds(train_950, "predictions/train_950.rds")
train_500 <- read_rds("predictions/train_500.rds")
train_050 <- read_rds("predictions/train_050.rds")
train_950 <- read_rds("predictions/train_950.rds")

# Plot fit
plot_fit <- data_frame(
  weeks = brand_awareness_investment %>% filter(year(weeks) < 2016) %>% .$weeks,
  `Awareness observado` = train_bai$awareness / 100,
  q05 = train_050$median / 100,
  `Mediana estimada` = train_500$median / 100,
  q95 = train_950$median / 100
) %>% 
  gather(awareness, value, `Awareness observado`, `Mediana estimada`)

interval_label <- paste0(
  "Estimación del intervalo de \n credibilidad para el \n Awareness, al ",  
  toString(100 * credibility),
  "%"
)

fit_review <- ggplot(data = plot_fit, aes(x = weeks)) +
  geom_line(aes(y = value, colour = awareness)) +
  geom_ribbon(
    aes(ymin = q05, ymax = q95, x = weeks, fill = interval_label),
    alpha = 0.3
  ) + 
  scale_fill_manual("", values = "grey12") +
  scale_colour_manual("", values = c("blue", "red")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("semanas") +
  ylab("awareness")

ggsave(
  filename = "results/fit_review.png",
  plot = fit_review,
  width = 12,
  height = 6
)

#### GOALS 2016 ####

# Predict
credibility <- 0.70
prediction_500 <- predict(GPDP_500, test_bai, credibility)
prediction_050 <- predict(GPDP_050, test_bai, credibility)
prediction_250 <- predict(GPDP_250, test_bai, credibility)
prediction_750 <- predict(GPDP_750, test_bai, credibility)
prediction_950 <- predict(GPDP_950, test_bai, credibility)

# Save/load prediction

# write_rds(prediction_500, "predictions/prediction_500.rds")
# write_rds(prediction_050, "predictions/prediction_050.rds")
# write_rds(prediction_250, "predictions/prediction_250.rds")
# write_rds(prediction_750, "predictions/prediction_750.rds")
# write_rds(prediction_950, "predictions/prediction_950.rds")
prediction_500 <- read_rds("predictions/prediction_500.rds")
prediction_050 <- read_rds("predictions/prediction_050.rds")
prediction_250 <- read_rds("predictions/prediction_250.rds")
prediction_750 <- read_rds("predictions/prediction_750.rds")
prediction_950 <- read_rds("predictions/prediction_950.rds")

plot_goals <- data_frame(
  weeks = test_bai$weeks,
  obs_awareness = test_bai$awareness / 100,
  lower75 = prediction_750$lower / 100,
  upper75 = prediction_750$upper / 100,
  lower05 = prediction_050$lower / 100,
  upper05 = prediction_050$upper / 100
) %>%
  mutate(title = "Awareness observado")

goals_2016 <- ggplot(data = plot_goals, aes(x = weeks)) +
  geom_line(aes(y = obs_awareness, colour = title)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual("",values = c("blue")) +
  geom_ribbon(
    aes(
      ymin = lower75,
      ymax = upper75,
      x = weeks,
      fill = "Awareness objetivo \n (Intervalo al 70% del \n cuantil 0.75-ésimo)"
    ),
    alpha = 0.3
  ) +
  geom_ribbon(
    aes(
      ymin = lower05,
      ymax = upper05,
      x = weeks,
      fill = "Intervalo al 70% del \n cuantil 0.05-ésimo"
    ),
    alpha = 0.3
  ) +
  scale_fill_manual("", values = c("grey12", "red"))  +
  xlab("semanas") +
  ylab("awareness") +
  theme(
    # legend.direction = 'horizontal', 
    # legend.position = 'bottom',
    legend.key = element_rect(size = 5),
    legend.key.size = unit(3, 'lines')
  )

ggsave(
  filename = "results/goals_2016.png",
  plot = goals_2016,
  width = 12,
  height = 6
)
