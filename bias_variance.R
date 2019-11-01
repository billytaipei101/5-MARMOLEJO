library(PairedData)

# As an example from: https://www.rdocumentation.org/packages/PairedData/versions/1.1.1/topics/rpaired.contaminated
rpaired2<- function (n, d1 = c(0.1, 10, 1), d2 = c(0.1, 10, 1), r = 0.5){
  #' F    the cumulative standard normal distribution
  #' eps  the percentage of contamination
  #' K    A scale parameter, usually sigma = 1
  #' 
  #' Test
  n  <- 100
  d1 <- c(0.1, 10, 1)
  d2 <- c(0.1, 10, 1)
  r  <- 0.7 
  
  eps1 <- d1[1]
  k1 <- d1[2]
  Sigma1 <- d1[3]
  eps2 <- d2[1]
  k2 <- d2[2]
  Sigma2 <- d2[3]
  X <- rmvnorm(n, mean = c(0, 0), sigma = matrix(c(1, r, r, 
                                                   1), ncol = 2))
  cor(X,method = 'kendall')
  u1 <- pnorm(X[, 1])
  b1 <- rbinom(n, size = 1, prob = eps1)
  SD1 <- Sigma1 * (b1 * k1 + (1 - b1) * 1)
  x <- qnorm(u1, mean = 0, sd = SD1)
  u2 <- pnorm(X[, 2])
  b2 <- rbinom(n, size = 1, prob = eps2)
  SD2 <- Sigma2 * (b2 * k2 + (1 - b2) * 1)
  y <- qnorm(u2, mean = 0, sd = SD2)
  return(paired(x, y))
}

m <- rpaired2(100,d1 = c(0.2, 10, 1), d2 = c(0.2, 10, 1), r = 0.5)
cor(m,method = 'kendall')
