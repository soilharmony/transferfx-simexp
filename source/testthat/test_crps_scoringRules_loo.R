library(testthat)

test_that("CRPS calculation matches between 'loo' and 'scoringRules'", {
  set.seed(11)
  
  N <- 1e4 # nr. observations
  K <- 4e3 # nr. draws from posterior predictive distribution
  
  y  <- rnorm(N) # observations
  x1 <- matrix(rnorm(N * K), nrow = K, ncol = N) # post. predictive draws
  x2 <- matrix(rnorm(N * K), nrow = K, ncol = N) # post. predictive draws
  
  # (1) scoringRules
  # scoringRules needs just a single matrix of draws for each observation
  crps_sr <- scoringRules::crps_sample(y = y, dat = t(x1))
  
  # (2) loo
  # loo needs two matrices with draws for each observation
  crps_loo <- loo::crps(x1, x2, y)
  
  # test that:
  expect_gte(cor(-crps_sr, crps_loo$pointwise), 0.99)
  expect_equal(-crps_sr, crps_loo$pointwise, tolerance = 0.01)
})