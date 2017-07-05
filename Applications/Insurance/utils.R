plot_sample <- function(sample_data, values_range, qerror, g_x) {
  # Join datasets to plot
  sample_plot <- sample_data %>%
    right_join(data_frame(x = values_range), by = "x") %>%
    mutate(
      q0.025 = g_x(x) + qerror(0.025),
      q0.500 = g_x(x) + qerror(0.500),
      q0.975 = g_x(x) + qerror(0.975)
    ) %>%
    gather(cuantil, value, c(q0.975, q0.500, q0.025))

  # Labels' order
  sample_plot$cuantil <- factor(
    sample_plot$cuantil,
    levels = c("q0.975", "q0.500", "q0.025"),
    ordered = TRUE
  )

  # Plot the simulated data
  sample_plot <- ggplot(data = sample_plot, aes(x = x)) +
    geom_line(aes(y = value, colour = cuantil)) +
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
    mutate(original = g_x(x) + qerror(p)) %>%
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
      q0.250 = g_x(x) + qerror(0.250),
      q0.500 = g_x(x) + qerror(0.500),
      q0.950 = g_x(x) + qerror(0.950)
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

zero_function <- function(x) return(0 * x)