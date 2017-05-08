# Preparando los datos

# load("C:/Users/mate/Downloads/I/sentimiento-master/probabilistic.topic/codigo/mydp/data/galaxy.rda")

rm(list=ls())
mywd <- "C:/Users/PardoO/Documents/Personal/Thesis/Code/"
setwd(mywd)

load(paste0(mywd,"galaxy.rda",set=""))

y <- unlist(galaxy)
rm(galaxy)

# Función slicer
KGWss <- function(y, d, zeta = 1.5^(-(1:1e3)), mcit = 1e4, burn = 2e3,
                  N = 10, thin = 10, alpha0 = 2, beta0 = 2, lambda = 15,
                  tau = 100, alpha = 1, beta = M, mu0 = mu0){
  # parametros iniciales
  n <- length(y)
  nz <- length(zeta)
  
  # repositorios
  out <- list()
  out$mu <- list()
  out$sigma <- list()
  out$N <- list()
  cont <- 1
  
  # Gibbs sampler
  cat(sprintf("Son %6d iteraciones.\n", mcit + burn))
  for(i in 1:(mcit+burn)){
    mu <- numeric(length = N)
    sigma <- numeric(length = N)
    v <- numeric(length = N)
    for(j in 1:N){
      indj <- d == j
      nj <- sum(indj)
      nnj <- (nj == 0) + (nj * (nj > 0))
      #aux <- sample_nig(mu = (0.15 * mu0 + sum(y[indj]))/(0.15 + nj),
      #                  lambda = 0.15 + nj,
      #                  alpha = alpha0 + nj/2,
      #                  beta = beta0 + (sum((y[indj] - sum(y[indj])/nnj)^2) +
      #                                ((0.15 * nj *
      #                                    (sum(y[indj])/nnj - mu0)^2)/
      #                                   (0.15 + nj)))/2)
      
      # Por modificar en nuestro contexto
      sigma[j] <- 1 / rgamma(1, shape = alpha0 + nj / 2,
                             rate = beta0 +
                               (sum((y[indj] - sum(y[indj])/nnj)^2) +
                                  (lambda * nj * (sum(y[indj])/nnj - mu0)^2)/
                                  (lambda + tau * nj))/2)
      mu[j] <- rnorm(1, mean = (mu0 * lambda + tau * sum(y[indj]))/(lambda + tau * nj),
                     sd = sqrt(tau * sigma[j]/(lambda + tau * nj)))
      
      # queda intacto
      v[j] <- rbeta(n = 1, shape1 = alpha + nj, shape2 = beta + sum(d > j))
    }
    
    # Pesos en el proceso Dirichlet
    w <- v * c(1, cumprod(1-v)[1:(N-1)])
    
    # Variables latentes de truncamiento
    u <- numeric(length = n)
    Ni <- numeric(length = n)
    
    for(j in 1:n){
      u[j] <- runif(n = 1, min = 0, max = zeta[d[j]])
      d[j] <- sample(x = 1:N, size = 1,
                     prob = (zeta[1:N] > u[j]) *
                       w / zeta[1:N] *
                       dnorm(y[j], mean = mu, sd = sqrt(sigma)))
      Ni[j] <- max((1:nz)[zeta > u[j]])
    }
    
    # Auxiliar para eliminar las simulaciones de calentamiento o no
    if(i > burn && (i - burn) %% thin == 0){
      out$mu[[cont]] <- mu
      out$sigma[[cont]] <- sigma
      out$N[[cont]] <- N
      cont <- cont + 1
    }
    if(i %% 1000 == 0){
      cat(sprintf("Van %6d iteraciones.\n", i))
    }
    N <- max(Ni)
  }
  out
}

# --------------------------
# Ejemplo
# --------------------------
# Preparando parámetros para reproducibilidad
set.seed(123)
burn <- 1e4
mc <- 3e4
thin <- 10
ittot <- mc / thin
M <- 10
alpha <- 1
beta <- M
mu0 <- mean(y)
alpha0 <- 2
beta0 <- 2
lambda <- 0.01
tau <- 1
d <- sample(1:10, size = length(y), replace = T)
N <- 10
zeta <- (0.5)*(0.5)^(0:1e3)

X <- KGWss(y = y, d = d, zeta = zeta, mcit = mc, burn = burn,
           N = N, thin = thin, alpha0 = alpha0, beta0 = beta0,
           lambda = lambda, tau = tau, alpha = alpha, beta = beta, 
           mu0 = mu0)

x <- seq(from = 5, to = 40, by  = 1e-3)
n <- length(x)
ye <- rep(0, n)

for(i in 1:ittot){
  s1 <- rep(0, n)
  for(j in X$N[[i]]){
    s1 <- s1 + dnorm(x, mean = X$mu[[i]][j], sd = sqrt(X$sigma[[i]][j]))
  }
  ye <- ye + (s1 / X$N[[i]])
  if( i %% 100 == 0 ){
    cat(sprintf("Van %5d iteraciones.\n", i))
  }
}
ye <- ye / ittot

plot(x, ye)
hist(y, breaks = 40, add = T, freq = F)
