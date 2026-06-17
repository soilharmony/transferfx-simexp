// error-in-variables model
//  with a prior on the SD of the measurement error

// based on this blog: https://statmodeling.stat.columbia.edu/2025/09/09/show-dont-tell-chatgpt-5-marginalizing-gelmans-measurment-error-model-in-stan/
// replaced the sigma_mex in the data block to a prior

data {
  int<lower=0> N;   // number of observations
  vector[N] y_obs;  // observed y
  vector[N] x_obs;  // observed x
}

parameters {
  // intercept of the relation between latent variables
  real b_Intercept;   
  // slope of the relation between latent variables 
  // (call it b_x_obs to work with brms-functions)
  real<lower=0> b_x_obs;
  // mean/SD of the latent X
  real mu_x;
  real<lower=0> sigma_x;
  // residual SD (y|x)
  real<lower=0> sigma;
  // SD of the measurement error in x
  real<lower=0> sigma_mex;
}

transformed parameters {
  real inv_var_x   = inv_square(sigma_x);
  real inv_var_mex = inv_square(sigma_mex);
  real tilde_v     = 1.0 / (inv_var_x + inv_var_mex);
  real<lower=0> sd_xobs = sqrt(square(sigma_x) + square(sigma_mex));
  real<lower=0> sd_yobs = sqrt(square(sigma) + square(b_x_obs) * tilde_v);
  vector[N] tilde_mu = tilde_v * (inv_var_x * rep_vector(mu_x, N)
                                  + inv_var_mex * x_obs);
  // lprior: needed for priorsense package
  real lprior; // joint prior
  real lprior_b_Intercept = normal_lpdf(b_Intercept | 0, 1); 
  real lprior_b_x_obs = normal_lpdf(b_x_obs | 1, .5);
  real lprior_mu_x = normal_lpdf(mu_x | 0, 1);
  real lprior_sigma_x = lognormal_lpdf(sigma_x | 0, .5);
  real lprior_sigma = lognormal_lpdf(sigma | 0, .5);
  real lprior_sigma_mex = lognormal_lpdf(sigma_mex | 0, .5);
  lprior = lprior_b_Intercept + lprior_b_x_obs + lprior_mu_x +
    lprior_sigma_x + lprior_sigma + lprior_sigma_mex;
}

model {
  // (hyper)priors
  mu_x        ~ normal(0, 1);
  sigma_x     ~ lognormal(0, 0.5);
  b_Intercept ~ normal(0, 1);
  b_x_obs     ~ normal(1, .5);
  sigma       ~ lognormal(0, 0.5);
  sigma_mex   ~ lognormal(0, 0.5);

  // marginalized likelihood
  x_obs ~ normal(mu_x, sd_xobs);
  y_obs ~ normal(b_Intercept + b_x_obs * tilde_mu, sd_yobs);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(
      y_obs[n] | b_Intercept + b_x_obs * tilde_mu[n], sd_yobs
    );
  }
}
