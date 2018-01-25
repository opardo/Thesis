plot_sample <- function(sample_data, values_range, qerror, g_x) {
  # Join datasets to plot
  sample_plot_df <- sample_data %>%
    right_join(data_frame(x = values_range), by = "x") %>%
    mutate(
      Mediana = as.factor("Mediana original de la \n distribución de los datos"),
      lower = g_x(x) + qerror(0.025, x),
      median = g_x(x) + qerror(0.500, x),
      upper = g_x(x) + qerror(0.975, x)
    )
  
  interval_label <- "Intervalo original de \n probabilidad de los datos, \n al 95%"

  # Plot the simulated data
  sample_plot <- ggplot(data = sample_plot_df, aes(x = x)) +
    geom_line(data = sample_plot_df, aes(y = median, group = Mediana, colour = Mediana)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper, x = x, fill = interval_label),
      alpha = 0.3
    ) +
    scale_colour_manual("", values = c("blue")) +
    scale_fill_manual("", values = "grey12") +
    geom_point(aes(y = y)) +
    xlab("x") +
    ylab("y")

  return(sample_plot)
}

plot_fitted_model <- function(
  prediction,
  credibility,
  g_x,
  sample_data,
  qerror,
  p,
  lower_limit,
  upper_limit
) {

  prediction <- prediction %>%
    select(-mean) %>%
    rename(`fitted median` = median) %>%
    mutate(original = g_x(x) + qerror(p,x)) %>%
    melt(c("x", "lower", "upper"))

  interval_label <- paste0(toString(100 * credibility), "% credible interval")

  # Get plot
  fitted_model_plot <- ggplot(data = prediction, aes(x = x)) +
    geom_line(aes(y = value, colour = variable)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper, x = x, fill = interval_label),
      alpha = 0.3
    ) +
    scale_colour_manual("", values = c("blue", "red")) +
    scale_fill_manual("", values = "grey12") +
    # geom_point(data = sample_data, aes(y = y), color = "black") +
    ggtitle(paste0("Cuantil ", p, "-ésimo")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("x") +
    ylab("y") +
    ylim(lower_limit, upper_limit) +
    theme(legend.position="none")

  return(fitted_model_plot)
}

plot_multiple_quantiles <- function(
  prediction_250,
  prediction_500,
  prediction_950,
  title
) {
  original <- prediction_250 %>%
    mutate(
      q0.250 = g_x(x) + qerror(0.250, x),
      q0.500 = g_x(x) + qerror(0.500, x),
      q0.950 = g_x(x) + qerror(0.950, x)
    ) %>%
    select(x, starts_with("q")) %>%
    gather(cuantil, value, -x)

  multiple_quantiles_plot <- ggplot(data = prediction_250, aes(x = x)) +
    geom_line(data = original, aes(y = value, colour = cuantil)) +
    scale_colour_manual("original", values = c("red", "red", "red")) +
    geom_line(data = prediction_250, aes(y = median), color = "blue") +
    geom_ribbon(
      data = prediction_250,
      aes(ymin = lower, ymax = upper, x = x, fill = "0.25"),
      alpha = 0.1
    ) +
    geom_line(data = prediction_500, aes(y = median), color = "green") +
    geom_ribbon(
      data = prediction_500,
      aes(ymin = lower, ymax = upper, x = x, fill = "0.50"),
      alpha = 0.1
    ) +
    geom_line(data = prediction_950, aes(y = median), color = "orange") +
    geom_ribbon(
      data = prediction_950,
      aes(ymin = lower, ymax = upper, x = x, fill = "0.95"),
      alpha = 0.1
    ) +
    scale_fill_manual("estimación (90%)", values = c("blue", "green", "orange")) +
    geom_point(data = sample_data, aes(y = y), color = "black")  +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("x") +
    ylab("y")

  return(multiple_quantiles_plot)
}

zero_function <- function(x) return(0.0 * x)

library(tibble)
library(dplyr)
library(pscl)

x <- sort(sample(seq(-15,15,0.01), 60))
y <- x + rnorm(length(x), 0 , 2)
plot(x,y)
data <- data_frame(x1=x, x2 = x^2, x3=x^3, x4=x^4, x5=x^5, y = y)
formula <- y ~ .
p <- 0.95
iter <- 1000

mf <- model.frame(formula = formula, data = data)
X <- model.matrix(attr(mf, "terms"), data = mf)
Y <- model.response(data = mf)

a <- 2
b <- 1
M <- matrix(rep(0, dim(X)[2]))
V <- diag(dim(X)[2])

V_ = qr.solve(qr.solve(V) + t(X) %*% X)
M_ = V_ %*% (solve(V,M) + t(X) %*% Y)
a_ = a + dim(X)[1] / 2
b_ = b + (t(M) %*% solve(V,M) + t(Y) %*% Y - t(M_) %*% solve(V_,M_)) / 2
b_/(a_-1)

Y_pred <- matrix(nrow=dim(X)[1], ncol=0)
for (i in 1:iter) {
  sigma <- rigamma(1, a_, b_)
  errors <- rnorm(dim(X)[1], 0, sigma)
  mean <- X %*% as.matrix(MASS::mvrnorm(1, M_, sigma * V_))
  y_pred <- mean + errors
  Y_pred <- cbind(Y_pred, y_pred)
}
Y_pred %>% apply(1, function(x) quantile(x, p))
