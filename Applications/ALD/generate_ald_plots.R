library(tibble)
library(ggplot2)
library(gridExtra)

# Define local_path where the repo lives (user dependent)
local_path <- "/Users/opardo/Documents/Projects/Personal/"

# Setwd for simple function and simple error folder
setwd(paste0(local_path,"Thesis/Applications/ALD/"))

save <- TRUE

dalp <- function(x, sigma, p) {
  kernel <- ifelse (
    x >= 0,
    exp(- p * x / sigma),
    exp((1 - p) * x / sigma)
  )
  d <- (p * (1 - p) / sigma) * kernel
  return(d)
}

x <- seq(-10,10,0.05)
p <- 0.25

changing_p <- data_frame(
  x = x,
  sigma0.5 = dalp(x, 0.5, p),
  sigma1 = dalp(x, 1, p),
  sigma2 = dalp(x, 2, p),
  sigma3 = dalp(x, 3, p)
)

plot_s0.5 <- ggplot(data = changing_p, aes(x = x, y = sigma0.5)) +
  geom_line() +
  ylim(c(0,0.4)) +
  ylab("fx") +
  ggtitle("sigma = 0.5") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

plot_s1 <- ggplot(data = changing_p, aes(x = x, y = sigma1)) +
  geom_line() +
  ylim(c(0,0.4)) +
  ylab("fx") +
  ggtitle("sigma = 1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

plot_s2 <- ggplot(data = changing_p, aes(x = x, y = sigma2)) +
  geom_line() +
  ylim(c(0,0.4)) +
  ylab("fx") +
  ggtitle("sigma = 2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

sigma_plots <- grid.arrange(
  plot_s0.5,
  plot_s1,
  plot_s2,
  ncol = 3
)

changing_p <- data_frame(
  x = x,
  p20 = dalp(x, 1, 0.20),
  p50 = dalp(x, 1, 0.5),
  p80 = dalp(x, 1, 0.80)
)

plot_p20 <- ggplot(data = changing_p, aes(x = x, y = p20)) +
  geom_line() +
  ylim(c(0,0.25)) +
  ylab("fx") +
  ggtitle("p = 0.20") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

plot_p50 <- ggplot(data = changing_p, aes(x = x, y = p50)) +
  geom_line() +
  ylim(c(0,0.25)) +
  ylab("fx") +
  ggtitle("p = 0.50") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")
       
plot_p80 <- ggplot(data = changing_p, aes(x = x, y = p80)) +
  geom_line() +
  ylim(c(0,0.25)) +
  ylab("fx") +
  ggtitle("p = 0.80") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

p_plots <- grid.arrange(
  plot_p20,
  plot_p50,
  plot_p80,
  ncol = 3
)

if (save) {
  ggsave(
    filename = "sigma_plots.png",
    plot = sigma_plots,
    width = 12,
    height = 5
  )
  ggsave(
    filename = "p_plots.png",
    plot = p_plots,
    width = 12,
    height = 5
  )
}