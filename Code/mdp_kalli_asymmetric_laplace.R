# Preparando los datos

# load("C:/Users/mate/Downloads/I/sentimiento-master/probabilistic.topic/codigo/mydp/data/galaxy.rda")
library(pscl)

rm(list=ls())
mywd <- "C:/Users/PardoO/Documents/Personal/Thesis/Code/"
setwd(mywd)

load(paste0(mywd,"galaxy.rda",set=""))

p <- 0.2

y <- unlist(galaxy) - quantile(unlist(galaxy), p)
rm(galaxy)

dalp <- function(x, sigma, p) {
  kernel <- ifelse (
    x >= 0,
    exp(- p * x / sigma),
    exp((1 - p) * x / sigma)
  ) 
  d <- (p * (1 - p) / sigma) * kernel
  return(d)
}

# Función slicer
KGWss <- function(
  y, d, zeta = 1.5^(-(1:1e3)), mcit = 1e4, burn = 2e3,
  N = 10, thin = 10, c_ig = 2, d_ig = 2, p =0.5, 
  alpha = 1, beta = M
){
  # parametros iniciales
  n <- length(y)
  nz <- length(zeta)
  
  # repositorios
  out <- list()
  out$sigma <- list()
  out$N <- list()
  cont <- 1
  
  # Gibbs sampler
  cat(sprintf("Son %6d iteraciones.\n", mcit + burn))
  for(i in 1:(mcit+burn)){
    sigma <- numeric(length = N)
    v <- numeric(length = N)
    for(j in 1:N){
      indj <- d == j
      indj_pos <- indj & (unname(y) >= 0)
      indj_neg <- indj & (unname(y) < 0)
      nj <- sum(indj)

      sigma[j] <- rigamma(
        1,
        c_ig + nj,
        d_ig + p * sum(y[indj_pos]) + (p - 1) * sum(y[indj_neg])
      )

      v[j] <- rbeta(n = 1, shape1 = alpha + nj, shape2 = beta + sum(d > j))
    }
    
    # Pesos en el proceso Dirichlet
    w <- v * c(1, cumprod(1-v)[1:(N-1)])
    
    # Variables latentes de truncamiento
    u <- numeric(length = n)
    Ni <- numeric(length = n)
    
    for(j in 1:n){
      u[j] <- runif(n = 1, min = 0, max = zeta[d[j]])
      d[j] <- sample(
        x = 1:N,
        size = 1,
        prob = (zeta[1:N] > u[j]) *
          w / zeta[1:N] *
          # w *
          dalp(y[j], sigma = sigma, p = p)
      )
      Ni[j] <- max((1:nz)[zeta > u[j]])
    }
    
    # Auxiliar para eliminar las simulaciones de calentamiento o no
    if(i > burn && (i - burn) %% thin == 0){
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

set.seed(1993)
zeta <- (0.5)*(0.5)^(0:1e3)
mcit <- 3e4
burn <- 1e4
N <- 50
thin <- 10
c_ig <- 1
d_ig <- 1
alpha <- 1
beta <- 5
d <- sample(1:N, size = length(y), replace = T)

X <- KGWss(
  y = y, d = d, zeta = zeta, mcit = mcit, burn = burn, N = N, 
  thin = thin, c_ig = c_ig, d_ig = d_ig, p = p, alpha = alpha, beta = beta
)

ittot <- mcit / thin
x <- seq(from = -20, to = 20, by  = 1e-3)
n <- length(x)
ye <- rep(0, n)

sigmas <- length(ittot)
for(i in 1:ittot){
  for(j in X$N[[i]]){
    sigmas[i] <- X$sigma[[i]][j]
  }
}

for(i in 1:ittot){
  s1 <- rep(0, n)
  for(j in X$N[[i]]){
    s1 <- s1 + dalp(x, sigma = X$sigma[[i]][j], p = p)
  }
  # ye <- ye + (s1 / X$N[[i]])
  ye <- ye + s1
  if( i %% 100 == 0 ){
    cat(sprintf("Van %5d iteraciones.\n", i))
  }
}
ye <- ye / ittot

plot(x, ye)
hist(y, breaks = 40, add = T, freq = F)
