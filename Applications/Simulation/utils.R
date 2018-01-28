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
  upper_limit,
  title = NULL
) {

  prediction <- prediction %>%
    select(-mean) %>%
    rename(`fitted median` = median) %>%
    mutate(original = g_x(x) + qerror(p,x)) %>%
    melt(c("x", "lower", "upper"))

  interval_label <- paste0(toString(100 * credibility), "% credible interval")
  
  if (is.null(title)) {
    title <- paste0("Cuantil ", p, "-ésimo")
  } else {
    title <- paste0(title," \n ","Cuantil ", p, "-ésimo")
  }
  
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
    ggtitle(title) +
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

cor_pred <- function(prediction, p) {
  return(cor(prediction$median, g_x(prediction$x) + qerror(p, prediction$x)) ^ 2)
}

mse_pred <- function(prediction, p) {
  return(mean((prediction$median - (g_x(prediction$x) + qerror(p, prediction$x))) ^ 2))
}

within_pred <- function(prediction, p) {
  y_real <- g_x(prediction$x) + qerror(p, prediction$x)
  return(mean(ifelse(y_real >= prediction$lower & y_real <= prediction$upper, 1, 0)))
}

gral_comp_matrix_metric <- function(tp_25, tp_50, tp_95, p_25, p_50, p_95, func) {
  mat <- matrix(nrow = 3, ncol = 2)
  rownames(mat) <- c("0.25","0.50","0.95")
  colnames(mat) <- c("Tradicional", "GPDP")
  mat[1,1] <- func(tp_25, 0.25)
  mat[2,1] <- func(tp_50, 0.50)
  mat[3,1] <- func(tp_95, 0.95)
  mat[1,2] <- func(p_25, 0.25)
  mat[2,2] <- func(p_50, 0.50)
  mat[3,2] <- func(p_95, 0.95)
  return(mat)
}
