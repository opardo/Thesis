################################################################################
################################## FUNCTIONS ###################################
################################################################################

# NEEDED LIBRARIES
library(pscl) # Inv-gamma
library(tmvtnorm) # Truncated Normal

# TOP LEVEL FUNCTION
MCMC_QuantGPDPReg <- function(
  X, 
  Y, 
  p = 0.5, 
  c_DP = 2, 
  d_DP = 1, 
  c_lambda = 2, 
  d_lambda = 0.5 * sd(Y),
  alpha = sqrt(length(Y)), 
  M = zero_function, 
  mcit = 3e4, 
  burn = 1e4, 
  thin = 10
){
  
  # Repositories
  output <- list()
  output$sigmas <- list()
  output$pis <- list()
  output$zetas <- list()
  output$N <- list()
  output$b <- list()
  output$lambda <- list()
  output$f <- list()
  output$bad <- list()
  cont <- 1
  
  # Fixed values
  xis <- (0.5)*(0.5)^(0:1e3)
  m <- dim(X)[1]
  n <- dim(X)[2]
  K_XX <- get_K_XX(X)
  K_XX_inv <- solve(K_XX)
  M_X <- M(X)
  
  # Initial values
  N <- 50
  zetas <- sample(1:N, size = length(Y), replace = T)
  f <- Y
  eps <- Y - f
  
  # Gibbs sampler
  cat(sprintf("Son %6d iteraciones.\n", mcit + burn))
  for(i in 1:(mcit+burn)){
    
    # Update Dirichlet Process
    DP_ks <- update_DP_ks(eps, zetas, p, c_DP, d_DP, alpha, N)
    sigmas <- update_sigmas(DP_ks)
    betas <- update_betas(DP_ks)
    pis <- turn_betas_into_pis(betas)
    
    # Update each observation's class
    classes <- update_classes(xis, zetas, pis, eps, sigmas, N, p, m)
    zetas <- update_zetas(classes)
    
    # Update Y >= f or Y < f
    b <- update_b(m, p, sigmas, zetas)
    
    # Update Gaussian Process
    try_f <- NULL
    global_chances <- 0
    while (global_chances < 3 & is.null(try_f) ) {
      
      try_lambda <- update_lambda(f, M_X, b, K_XX, K_XX_inv, n, c_lambda, d_lambda)
      
      f_chances <- 0
      while(f_chances < 4 & is.null(try_f)) {
        try_f <- update_f(Y, M_X, K_XX, try_lambda, b)
        f_chances <- f_chances + 1 
      }
      
      global_chances <- global_chances + 1
    }
    
    if(is.null(try_f)) {
      f <- Y - random_asymmetric_error(sigmas[zetas], p)
      lambda <- update_lambda(f, M_X, b, K_XX, K_XX_inv, n, c_lambda, d_lambda)
      bad <- 1
    } else {
      lambda <- try_lambda
      f <- try_f
      bad <- 0
    }
    
    eps <- Y-f
    
    # Aux to delete burning simulations
    if(i > burn && (i - burn) %% thin == 0){
      output$sigmas[[cont]] <- sigmas
      output$pis[[cont]] <- pis
      output$zetas[[cont]] <- zetas
      output$N[[cont]] <- N
      output$b[[cont]] <- b
      output$lambda[[cont]] <- lambda
      output$f[[cont]] <- f
      output$bad[[cont]] <- bad
      cont <- cont + 1
    }
    
    if(i %% 1000 == 0){
      cat(sprintf("Van %6d iteraciones.\n", i))
    }
    
    # Update number of classes truncation
    N <- update_N(classes)
  }
  return(output)
}

# GENERAL FUNCTIONS
zero_function <- function(x) return(0 * x)

# asymmetric Laplace density
dalp <- function(x, sigma, p) {
  kernel <- ifelse (
    x >= 0,
    exp(- p * x / sigma),
    exp((1 - p) * x / sigma)
  ) 
  d <- (p * (1 - p) / sigma) * kernel
  return(d)
}

# Asymmetric Laplace random
ralp <- function(n, sigma = 1, p = 0.5) {
  x <- seq(-10, 10, 0.05)
  prob <- dalp(x, sigma, p)
  sample_alp <- sample(
    x = x,
    size = n,
    prob = prob
  )
  return(sample_alp)
}

# Generate random Laplace asymmetric vector
random_asymmetric_error <- function(sigmas, p) {
  unique_sigmas <- unique(sigmas)
  error <- vector(length = length(sigmas))
  
  for (sigma in unique_sigmas) {
    dim <- sum(sigmas == sigma)
    simulations <- ralp(dim, sigma = sigma, p)
    error[sigmas == sigma] <- simulations
  }
  
  return(error)
}

# Correlation matrix
get_K_XX <- function(X) {
  m <- dim(X)[1]
  K <- matrix(nrow = m, ncol = m)
  for(i in 1:m) {
    for(j in 1:m){
      K[i,j] <- exp(-as.numeric(dist(rbind(X[i,],X[j,]))))
    }
  }
  return(K)
}

update_DP_k <- function(k, eps, zetas, p, c_DP, d_DP, alpha) {
  ind <- zetas == k
  ind_pos <- ind & (unname(eps) >= 0)
  ind_neg <- ind & (unname(eps) < 0)
  r <- sum(ind)
  
  sigma_k <- rigamma(
    1,
    c_DP + r,
    d_DP + p * sum(eps[ind_pos]) + (1-p) * sum(-eps[ind_neg])
  )
  
  beta_k <- rbeta(n = 1, shape1 = 1 + r, shape2 = alpha + sum(zetas > k))
  
  return(list(
    sigma_k = sigma_k,
    beta_k = beta_k
  ))
}

# DIRICHLET PROCESS FUNCTIONS

# Update Dirichlet Process
update_DP_ks <- function(eps, zetas, p, c_DP, d_DP, alpha, N) {
  return(lapply(
    1:N,
    update_DP_k,
    eps = eps,
    zetas = zetas,
    p = p,
    c_DP = c_DP,
    d_DP = d_DP,
    alpha = alpha
  ))
}

update_sigmas <- function(DP_ks) unlist(
  lapply(DP_ks, function(DP_k) DP_k$sigma_k)
)

update_betas <- function(DP_ks) unlist(
  lapply(DP_ks, function(DP_k) DP_k$beta_k)
)

turn_betas_into_pis <- function(betas) {
  pis_hat <- betas * c(1, cumprod(1-betas)[1:(length(betas)-1)])
  pis <- pis_hat / sum(pis_hat)
  return(pis)
}

# CLASSES FUNCTIONS

update_class_i <- function(i, xis, zetas, pis, eps, sigmas, N, p) {
  u <- runif(n = 1, min = 0, max = xis[zetas[i]])
  if(sum(is.na((xis[1:N] > u) * pis / xis[1:N] * dalp(eps[i], sigma = sigmas, p = p))) > 0) browser()
  zeta_i <- sample(
    1,
    x = 1:N,
    prob = (xis[1:N] > u) *
      pis / xis[1:N] *
      dalp(eps[i], sigma = sigmas, p = p)
  )
  N_i <- max((1:length(xis))[xis > u])
  return(list(
    zeta_i = zeta_i,
    N_i = N_i
  ))
}

update_classes <- function(xis, zetas, pis, eps, sigmas, N, p, m) {
  return(lapply(
    1:m,
    update_class_i,
    xis = xis, 
    zetas = zetas, 
    pis = pis, 
    eps = eps, 
    sigmas = sigmas, 
    N = N, 
    p = p
  ))
} 

update_zetas <- function(classes) unlist(
  lapply(classes, function(class) class$zeta_i)
)

update_N <- function(classes) max(unlist(
  lapply(classes, function(class) class$N_i)
))

# GAUSSIAN PROCESS FUNCTIONS

update_b <- function(m, p, sigmas, zetas) {
  return(ifelse(runif(m) > p, p / sigmas[zetas], -(1-p) / sigmas[zetas]))
}

update_f <- function(Y, M_X, K_XX, lambda, b) {
  f <- rtmvnorm(
    n = 1,
    mean = as.vector(M_X + (lambda * K_XX) %*% b),
    sigma = lambda * K_XX,
    lower = ifelse(b >= 0, -Inf, Y),
    upper = ifelse(b >= 0, Y, Inf),
    algorithm = "gibbs"
  )[1,]
  if(any(is.nan(f)) > 0 | any(is.infinite(f)) > 0) {
    f <- NULL
  }
  return(f)
}

update_lambda <- function(f, M_X, b, K_XX, K_XX_inv, n, c_lambda, d_lambda) {
  
  bar_F <- 1/2 * as.numeric(t(f-as.vector(M_X)) %*% K_XX_inv %*% (f-as.vector(M_X)))
  bar_B <- 1/2 * as.numeric(t(b) %*% K_XX %*% b)
  bar_c <- c_lambda + n/2
  bar_d <- d_lambda + bar_F
  
  lambdas <- seq(0.005, 5, 0.005)
  fx <- function(lambda) {
    return(lambda ^ (-(bar_c + 1)) * exp(- bar_d / lambda - bar_B * lambda))
  }
  prob <- fx(lambdas)
  
  if (sum(prob > 0) > 0) {
    lambda <- sample(x = lambdas[prob > 0], size = 1, prob = prob[prob > 0])
  } else {
    lambda <- rigamma(1, c_lambda, d_lambda)
  }
  return(lambda) 
}

################################################################################
################################# TEST SCRIPT ##################################
################################################################################

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
MCMC_output <- MCMC_QuantGPDPReg(
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
  ggtitle(paste0("Quantile = ", p)) +
  theme(plot.title = element_text(hjust = 0.5))


# Save fitted model

library(readr)
setwd("C:/Users/PardoO/Documents/Personal/Thesis/Code")
write_rds(MCMC_output, paste0("MCMC_QuantGPDPReg_",as.character(100*p),".rds"))
