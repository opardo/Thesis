TradQuantReg <- function(data, formula) {
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(attr(mf, "terms"), data = mf)
  Y <- model.response(data = mf)
  
  a <- 2
  b <- 1
  M <- matrix(rep(0, dim(X)[2]))
  V <- diag(dim(X)[2])
  
  post_parameters <- list()
  
  V_ = qr.solve(qr.solve(V) + t(X) %*% X)
  M_ = V_ %*% (solve(V,M) + t(X) %*% Y)
  a_ = a + dim(X)[1] / 2
  b_ = b + (t(M) %*% solve(V,M) + t(Y) %*% Y - t(M_) %*% solve(V_,M_)) / 2
  
  post_parameters$V_ <- V_
  post_parameters$M_ <- M_
  post_parameters$a_ <- a_
  post_parameters$b_ <- b_
  post_parameters$formula <- formula
  
  return(post_parameters)
}

TradQuantReg_simulate <- function(sigma, X, M_, V_, p) {
  errors <- qnorm(p, 0, sigma)
  mean <- X %*% as.matrix(MASS::mvrnorm(1, M_, sigma * V_))
  y_preds <- lapply(errors, function(error) mean + error)
  names(y_preds) <- p
  return(y_preds)
}

TradQuantReg_intervals <- function(Y_pred, X, credibility) {
  p_lower <- (1 - credibility) / 2
  p_upper <- 1 - p_lower
  results <- data.frame(
    mean = Y_pred %>% apply(2, mean) %>% unname,
    median = Y_pred %>% apply(2, median) %>% unname,
    lower = Y_pred %>% apply(2, function(col) quantile(col, p_lower)) %>% unname,
    upper= Y_pred %>% apply(2, function(col) quantile(col, p_upper)) %>% unname
  )
  prediction <- cbind(X, results)
  return(prediction)
}

TradQuantReg_p_prediction <- function(pi, y_preds_list, X, credibility) {
  y_preds_p <- lapply(y_preds_list, function(y_preds) y_preds[[pi]])
  results_p <- data.frame(matrix(
    unlist(y_preds_p),
    nrow = length(y_preds_p),
    byrow = T
  )) %>% 
    TradQuantReg_intervals(X, credibility)  
  return(results_p)
}

TradQuantReg_predict <- function(post_parameters, data, p, credibility, iter = 3000) {
  
  V_ <- post_parameters$V_
  M_ <- post_parameters$M_
  a_ <- post_parameters$a_
  b_ <- post_parameters$b_
  formula <- post_parameters$formula
  
  mf <- model.frame(formula = formula[-2], data = data)
  X <- model.matrix(attr(mf, "terms"), data = mf)
  
  sigmas <- rigamma(iter, a_, b_)
  registerDoParallel()
  y_preds_list <- foreach(sigma = sigmas) %dopar% {
    TradQuantReg_simulate(sigma, X, M_, V_, p)
  }
  
  predictions <- lapply(p, TradQuantReg_p_prediction, y_preds_list, X, credibility)
  names(predictions) <- p
  
  return(predictions)
}
