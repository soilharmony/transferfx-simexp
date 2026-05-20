// basic linear regression model

data {
  int<lower=1> N;   // number of observations
  vector[N] x_obs;  // observed x
  vector[N] y_obs;  // observed y
}

parameters {
  real b_Intercept;       // intercept
  real<lower=0> b_x_obs;  // slope
  real<lower=0> sigma;    // residual sd
}

model {
  // prior
  b_Intercept ~ normal(0, 1);
  b_x_obs     ~ normal(1, .5);
  sigma       ~ lognormal(0, 0.5);
  // likelihood
  y_obs ~ normal(b_Intercept + b_x_obs * x_obs, sigma);
}

generated quantities {
  // not used for now (needed for WAIC)
  // vector[N] y_rep = to_vector(
  //   normal_rng(b_Intercept + b_x_obs * x_obs, sigma)
  // );
  // vector[N] log_lik = normal_lpdf(
  //   y_obs | b_Intercept + b_x_obs * x_obs, sigma
  // );
}
