// multivariate error-in-variables regression
//  with known covariance of the measurement error in the (K-1)-dim predictor

// written by Claude-AI


data {
  int<lower=0> N;                 // nr. observations
  int<lower=3> K;                 // K-dimensional simplex (3 in our case)
  array[N] vector[K-1] ilr_y_obs; // outcome (ref-method)
  array[N] vector[K-1] ilr_x_obs; // predictor (nonref-method)
  cov_matrix[K-1] Sigma_mex;      // KNOWN covariance matrix of me-x on ILR-scale
  // new data for prediction only (not used in model fitting)
  int<lower=0> N_new;
  array[N_new] vector[K-1] ilr_x_obs_new;
}

transformed data {
  int Kx = K - 1;
  matrix[Kx, Kx] Sigma_mex_inv = inverse_spd(Sigma_mex);
}

parameters {
  // regression coefficients: column 1 = intercept, columns 2..K = slopes on x
  matrix[Kx, K] beta;

  // residual (y|x) covariance, via Cholesky of the correlation matrix
  cholesky_factor_corr[Kx] L_Omega_y;
  vector<lower=0>[Kx] l_sigma_y;

  // latent-x distribution: mean and covariance (also via Cholesky)
  vector[Kx] mu_x;
  cholesky_factor_corr[Kx] L_Omega_x;
  vector<lower=0>[Kx] l_sigma_x;
}

transformed parameters {
  matrix[Kx, Kx] L_Sigma_y = diag_pre_multiply(l_sigma_y, L_Omega_y);
  matrix[Kx, Kx] Sigma_y   = multiply_lower_tri_self_transpose(L_Sigma_y);

  matrix[Kx, Kx] L_Sigma_x   = diag_pre_multiply(l_sigma_x, L_Omega_x);
  matrix[Kx, Kx] Sigma_x     = multiply_lower_tri_self_transpose(L_Sigma_x);
  matrix[Kx, Kx] Sigma_x_inv = inverse_spd(Sigma_x);

  // b0 = intercept vector, B = slope block acting on x
  vector[Kx] b0 = beta[, 1];
  matrix[Kx, Kx] B = beta[, 2:K];

  // --- analytic marginalization over the latent x ---
  matrix[Kx, Kx] Sigma_xobs = Sigma_x + Sigma_mex;              // Var(x_obs)
  matrix[Kx, Kx] tilde_V_inv = Sigma_x_inv + Sigma_mex_inv;     // precision of x | x_obs
  matrix[Kx, Kx] tilde_V = inverse_spd(tilde_V_inv);            // Var(x | x_obs)
  matrix[Kx, Kx] Sigma_y_marg = quad_form(tilde_V, B') + Sigma_y; // Var(y | x_obs)

  // Cholesky factors for the two marginal MVN likelihoods
  matrix[Kx, Kx] L_Sigma_xobs   = cholesky_decompose(Sigma_xobs);
  matrix[Kx, Kx] L_Sigma_ymarg  = cholesky_decompose(Sigma_y_marg);

  // E[x | x_obs] for every observation: tilde_mu_i = tilde_V (Sx^-1 mu_x + Smex^-1 x_obs_i)
  array[N] vector[Kx] tilde_mu;
  {
    vector[Kx] prior_term = Sigma_x_inv * mu_x;
    for (i in 1:N) {
      tilde_mu[i] = tilde_V * (prior_term + Sigma_mex_inv * ilr_x_obs[i]);
    }
  }
}

model {
  // (hyper)priors
  mu_x            ~ normal(0, 1);
  L_Omega_x       ~ lkj_corr_cholesky(4);
  l_sigma_x       ~ cauchy(0, 2.5);

  L_Omega_y       ~ lkj_corr_cholesky(4);
  l_sigma_y       ~ cauchy(0, 2.5);
  to_vector(beta) ~ normal(0, 10);

  // marginal likelihood of x_obs (integrates out x)
  ilr_x_obs ~ multi_normal_cholesky(mu_x, L_Sigma_xobs);

  // marginal likelihood of y given x_obs (integrates out x)
  {
    array[N] vector[Kx] Mu_y;
    for (i in 1:N) Mu_y[i] = b0 + B * tilde_mu[i];
    ilr_y_obs ~ multi_normal_cholesky(Mu_y, L_Sigma_ymarg);
  }
}

generated quantities {
  // posterior predictive draws for new (error-free, or already-in-hand) x
  // here treated the same way as mvlinreg.stan: predicting from ilr_x_obs_new
  // by first shrinking it through the same measurement-error update.
  array[N_new] vector[Kx] ilr_y_obs_new_rep;
  {
    vector[Kx] prior_term = Sigma_x_inv * mu_x;
    for (i in 1:N_new) {
      vector[Kx] tilde_mu_new = tilde_V * (prior_term + Sigma_mex_inv * ilr_x_obs_new[i]);
      ilr_y_obs_new_rep[i] = multi_normal_cholesky_rng(
        b0 + B * tilde_mu_new, L_Sigma_ymarg
      );
    }
  }
}
