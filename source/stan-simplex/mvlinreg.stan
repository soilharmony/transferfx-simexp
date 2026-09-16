// multivariate linear regression
// predictor = intercept + 2-dim ILR coordinates (nonref-method)
// outcome   = 2-dim ILR coordinates (ref-method)

data {
  int<lower=0> N; // nr. observations
  int<lower=3> K; // K-dimensional simplex (3 in our case)
  array[N] vector[K-1] ilr_y_obs;
  array[N] vector[K-1] ilr_x_obs;
  // new data for prediction only (not used in model fitting)
  int<lower=0> N_new;
  array[N_new] vector[K-1] ilr_x_obs_new;
}

transformed data {
  // append an intercept column of 1's to ilr_x_obs
  array[N] vector[K] ilr1_x_obs;
  for (i in 1:N) {
    ilr1_x_obs[i] = to_vector(append_col(1, to_row_vector(ilr_x_obs[i])));
  }
  array[N_new] vector[K] ilr1_x_obs_new;
  for (i in 1:N_new) {
    ilr1_x_obs_new[i] = to_vector(append_col(1, to_row_vector(ilr_x_obs_new[i])));
  }
}

parameters {
  // regression coefficients
  // full model = intercept + 2 coefs for each ilr-predictor = 3 columns
  matrix[K-1, K] beta;   
  // residuals: 2-dimensional via Cholesky decomp. of the corr-matrix
  cholesky_factor_corr[K-1] L_Omega; 
  vector<lower=0>[K-1] l_sigma; 
}

transformed parameters {
  // get the covariance matrix from the Cholesky-factorization
  matrix[K-1, K-1] L_Sigma = diag_pre_multiply(l_sigma, L_Omega);
}

model {
  // prior
  L_Omega         ~ lkj_corr_cholesky(4);
  l_sigma         ~ cauchy(0, 2.5);
  to_vector(beta) ~ normal(0, 10);
  // mean-part of the regression model
  array[N] vector[K-1] Mu;
  for (i in 1:N) {
    Mu[i] = beta * ilr1_x_obs[i];
  }
  // likelihood
  ilr_y_obs ~ multi_normal_cholesky(Mu, L_Sigma);
}

generated quantities {
  // posterior predictive draws for predictions in new data
  array[N_new] vector[K-1] ilr_y_obs_new_rep;
  for (i in 1:N_new) {
    ilr_y_obs_new_rep[i] = multi_normal_cholesky_rng(
      beta * ilr1_x_obs_new[i], L_Sigma
    );
  }
}
