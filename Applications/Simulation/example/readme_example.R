library(devtools)
install_github("opardo/GPDPQuantReg")
library(GPDPQuantReg)

ptm <- proc.time
set.seed(201707)
f_x <- function(x) return(0.5 * x * cos(x) - exp(0.1 * x))
error <- function(m) rgamma(m, 2, 1)
m <- 20
x <- sort(sample(seq(-15, 15, 0.005), m))
sample_data <- data.frame(x = x, y = f_x(x) + error(m))

GPDP_MCMC <- GPDPQuantReg(y ~ x, sample_data, p = 0.250)

diagnose(GPDP_MCMC)

predictive_data <- data_frame(x = seq(-15, 15, 0.25))
credibility <- 0.90
prediction <- predict(GPDP_MCMC, predictive_data, credibility)
proc.time() - ptm