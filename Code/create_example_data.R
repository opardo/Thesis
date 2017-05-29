library(ggplot2)
library(tibble)

set.seed(19930318)

f_x <- function(x) return(10*sin(x/2) +  x - 2 * log(1/x))
error <- function(m, sigma) rnorm(m, 0, sigma)

m <- 30
sigma <- 4
x_range <- seq(1, 20, 0.05)
x <- sample(x_range, m)

original_data <- data_frame(
  x = x_range,
  y = f_x(x_range)
)

sample_data <- data_frame(
  x = x,
  y = f_x(x) + error(m, sigma)
)

ggplot(data = original_data, aes(x = x)) +
  geom_line(aes(y = y), color = "red") +
  geom_line(aes(y = y + qnorm(0.80, 0, sigma)), color = "blue") +
  geom_line(aes(y = y + qnorm(0.20, 0, sigma)), color = "blue") +
  geom_point(data = sample_data, aes(y = y))

