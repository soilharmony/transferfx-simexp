// dirichlet.stan
// Dirichlet regression for K-dimensional compositional data (K = 3 by default).
// Each row of y is a simplex (non-negative, sums to 1)

data {
  int<lower=1> N;  // number of observations (rows)
  int<lower=3> K;  // number of composition categories (use K = 3)
  matrix[N, K] psd_x_obs;        // predictor matrix, N x K (excl. intercept)
  array[N] simplex[K] psd_y_obs; // outcomes: N simplices of length K
}

transformed data {
  // Prepend an intercept column of 1s to X
  matrix[N, K + 1] psd1_x_obs = append_col(rep_vector(1.0, N), psd_x_obs);
}

parameters {
  // One coefficient vector (length K+1, incl. intercept) per category
  matrix[K + 1, K] beta;
}

transformed parameters {
  array[N] vector[K] alpha;
  for (n in 1:N) {
    alpha[n] = exp(to_vector(psd1_x_obs[n] * beta));
  }
}

model {
  // priors
  to_vector(beta) ~ normal(0, 5);
  // likelihood
  for (n in 1:N) {
    psd_y_obs[n] ~ dirichlet(alpha[n]);
  }
}

generated quantities {
  //
}
