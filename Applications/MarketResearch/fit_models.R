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

# Import utils
# source(paste0(local_path,"Thesis/Applications/Simulation/utils.R"), local = TRUE)

# Setwd for simple function and simple error folder
setwd(paste0(local_path,"Thesis/Applications/MarketResearch/"))

brand_awareness_investment <- read_csv("brand_awareness_investment.csv") %>% 
  mutate(weeks = dmy(weeks))

train_bai <- brand_awareness_investment %>% 
  filter(year(weeks) < 2016) %>% 
  select(-weeks)

test_bai <- brand_awareness_investment %>% 
  filter(year(weeks) == 2016) %>% 
  select(-weeks)

model <- lm(awareness ~ ., data = train_bai)
summary(model)

compare_lr_prediction <- data_frame(
  weeks = brand_awareness_investment$weeks,
  original = brand_awareness_investment$awareness,
  predicted = predict(model, newdata = brand_awareness_investment)
) %>% 
  gather(awareness, value, -weeks)

ggplot(data = compare_lr_prediction, aes(x = weeks, colour = awareness)) +
  geom_line(aes(y = value))

set.seed(2017)

GPDP_500 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.500, d_DP = 2, d_lambda = 2)
GPDP_750 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.750, d_DP = 2, d_lambda = 2)
GPDP_900 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.900, d_DP = 2, d_lambda = 2)
GPDP_950 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.950, d_DP = 2, d_lambda = 2)
GPDP_975 <- GPDPQuantReg(awareness ~ ., train_bai, p = 0.975, d_DP = 2, d_lambda = 2)
