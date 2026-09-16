// multivariate linear regression

data {
  int<lower=0> N; // nr. observations
  int<lower=0> K; // 2-dimensional outcome variable (ILR of ref method)
  int<lower=0> J; // 3-dimensional predictor variable (intercept + ILR of non-ref method)
  array[N] vector[K] ilr_y_obs;
  array[N] vector[J] ilr_x_obs;
  // new data for prediction only (not used in model fitting)
  int<lower=0> N_new;
  array[N_new] vector[J] ilr_x_obs_new;
}

transformed data {
  //
}

parameters {
  // regression coefficients
  matrix[K, J] beta;   
  // residuals: 2-dimensional via Cholesky decomp. of the corr-matrix
  cholesky_factor_corr[K] L_Omega; 
  vector<lower=0>[K] l_sigma; 
}

transformed parameters {
  // get the covariance matrix from the Cholesky-factorization
  matrix[K, K] L_Sigma = diag_pre_multiply(l_sigma, L_Omega);
}

model {
  // prior
  L_Omega ~ lkj_corr_cholesky(4);
  l_sigma ~ cauchy(0, 2.5);
  to_vector(beta) ~ normal(0, 10);
  // mean-part of the regression model
  array[N] vector[K] Mu;
  for (i in 1:N) {
    Mu[i] = beta * ilr_x_obs[i];
  }
  // likelihood
  ilr_y_obs ~ multi_normal_cholesky(Mu, L_Sigma);
}

generated quantities {
  // mean-part of the regression model (predictions only)
  array[N_new] vector[K] Mu_new;
  for (i in 1:N_new) {
    Mu_new[i] = beta * ilr_x_obs_new[i];
  }
  // posterior predictive draws for the predictions
  array[N_new] vector[K] ilr_y_obs_new_rep;
  for (i in 1:N_new) {
    ilr_y_obs_new_rep[i] = multi_normal_cholesky_rng(Mu_new[i], L_Sigma);
  }
} 
