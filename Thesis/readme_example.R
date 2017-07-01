# Instalación del paquete
install.packages("devtools")
library(devtools)
install_github("opardo/GPDPQuantReg")
library(GPDPQuantReg)

# Simulación de datos 
set.seed(201707)
f_x <- function(x) return(0.5 * x * cos(x) - exp(0.1 * x))
error <- function(m) rgamma(m, 2, 1)
m <- 20
x <- sort(sample(seq(-15, 15, 0.005), m))
sample_data <- data.frame(x = x, y = f_x(x) + error(m))

# Ajuste del modelo
GPDP_MCMC <- GPDPQuantReg(y ~ x, sample_data, p = 0.250)

# Predicción, usando el modelo ajustado
pred_data <- data.frame(x = seq(-15, 15, 0.25))
credibility <- 0.90
prediction <- predict(GPDP_MCMC, pred_data, credibility)

# Diagnóstico de las cadenas de markov
diagnose(GPDP_MCMC)