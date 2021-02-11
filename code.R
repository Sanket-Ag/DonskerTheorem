library(stats)

t <- seq(0, 1, length.out = 1e3)

N <- c(seq(1:10), seq(20, 100, by = 10), seq(200, 1000, by = 100), seq(2e3, 1e4, by = 1e3))
output <- matrix(0, nrow = length(N), ncol = length(t))

prev <- 0
Z <- numeric()
for(i in 1:length(N)){
  n <- N[i] - prev
  Z <- c(Z, runif(n, min = 0, max = 1))

  cdf <- ecdf(Z)
  fn_cap <- cdf(t)
  y <- sqrt(N[i])*(fn_cap - t)

  prev <- n
  output[i, ] <- y
}
