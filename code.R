#### Required packages: stats, ggplot2, ggpanimate, gifski, png, transformr

library(stats)
library(gganimate)
set.seed(4)

# This code visualizes classical Donsker's Theorem for random samples from U(0, 1) distribution.
# Gif shows the convergence of sqrt(n)(F_n(t) - t) to a Brownian bridge process as n increases.
# Here F_n(.) is the empirical distribution function based on a random sample of size n.

t <- seq(0, 1, length.out = 1e3)  # x-axis

N <- c(seq(1:10), seq(20, 100, by = 10), seq(200, 1000, by = 100), seq(2e3, 1e4, by = 1e3)) # Different values of n (sample size)
output <- matrix(0, nrow = length(N), ncol = length(t)) # Matrix to contain process values

Z <- runif(N[length(N)], min = 0, max = 1)

for(i in 1:length(N)){

  cdf <- ecdf(Z[1:N[i]])
  fn_cap <- cdf(t)
  output[i, ] <- sqrt(N[i])*(fn_cap - t)
  
}

#################################################
#################################################
# For animation

T <- rep(t, length(N))
n <- factor(rep(N, each = 1e3))
value <- numeric()

for(i in 1:length(N)){
  value <- c(value, output[i, ])
}

data <- data.frame(T, n, value)

p <- ggplot(data, aes(x = T, y = value)) + geom_path()
anim <- p + transition_states(n, transition_length = 1, state_length = 2, wrap = TRUE)
anim <- anim + ggtitle('N = {closest_state}')
anim
