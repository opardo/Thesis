plot_fitted_model <- function(
  prediction,
  credibility,
  original_data,
  sample_data,
  qerror, 
  params, 
  p
) {
  
  prediction <- prediction %>% 
    rename(x = Xp, `fitted mean` = mean, `fitted median` = median) %>% 
    mutate(x = round(x, 3))
  
  original_data <- original_data %>% 
    mutate(
      x = round(x, 3),
      original = f + qerror(p, params)
    ) %>% 
    select(-f)
  
  # Generate comparisson data frame
  comparisson <- left_join(
    prediction,
    original_data,
    by = "x"
  )
  comparisson <- melt(comparisson, c("x", "lower", "upper"))
  
  interval_title <- paste0(toString(100 * credibility), "% credible interval")
  
  # Get plot
  fitted_model_plot <- ggplot(data = comparisson, aes(x = x)) +
    geom_line(aes(y = value, colour = variable)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper, x = x, fill = interval_title),
      alpha = 0.3
    ) +
    scale_colour_manual("", values = c("blue", "green", "red")) +
    scale_fill_manual("", values = "grey12") +
    geom_point(data = sample_data, aes(y = y), color = "black") +
    ggtitle(paste0("Quantile = ", p)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("x") + 
    ylab("y") 
  
  return(fitted_model_plot)
}