// gammareg-leftcens.stan
// STAN model for TF on left-censored gamma-distributed soil descriptors

data {
  int<lower=0> N;     // number of observations
  
  // unused parameters:
  //real<lower=0> lodx; // LoD (predictor variable)
  //real<lower=0> lody; // LoD (outcome variable)
  //int<lower=0, upper=N> Nobsy;  // number of non-censored observations
  //int<lower=0, upper=N> Nobsx;  
  //int<lower=0, upper=N> Nceny;  // number of censored observations
  //int<lower=0, upper=N> Ncenx; 
  
  // observed variables (= LoD for censored observations)
  array[N] real<lower=0> x_obs_c; 
  array[N] real<lower=0> y_obs_c; 
  // censoring indicators: -1 = left-censored, 0 = uncensored
  array[N] int<lower=-1, upper=0> x_obs_ci; 
  array[N] int<lower=-1, upper=0> y_obs_ci;
  
  // new observations in the validation data
  int<lower=0> N_val;
  array[N_val] real<lower=0> x_obs_c_val; 
  array[N_val] int<lower=-1, upper=0> x_obs_ci_val; 
}

transformed data {
  
}

parameters {
  // latent censored predictor
  real<lower=0> mu_x;
  real<lower=0> cv_x;
  
  // gamma-GLM regression model
  real<lower=0> shape;
  real beta0;
  real<lower=0> beta1;
}

transformed parameters {
  // latent censored predictor
  real<lower=0> shape_x = inv_square(cv_x);
  real<lower=0> rate_x  = shape_x / mu_x;
}

model {
  // prior on latent predictor
  //mu_x ~ ; flat priors for the moment
  //cv_x ~ ;
  
  // prior on gamma-GLM parameters
  beta0 ~ normal(0, 10);
  beta1 ~ normal(1, .5);
  shape ~ lognormal(0, 1);
  
  // loop over observations
  array[N] real mu;
  for (i in 1:N) {
    
    // contribution of the predictor to the log-posterior
    if (x_obs_ci[i] == 0) {
      // observed predictor: PDF
      x_obs_c[i] ~ gamma(shape_x, rate_x);
    } else if (x_obs_ci[i] == 1) {
      // left-censored predictor: integrate out with CDF
      target += gamma_lcdf(x_obs_c[i] | shape_x, rate_x);
    }
    
    // mean-part of gamma-GLM
    mu[i] = exp(beta0 + beta1 * log(x_obs_c[i]));
    
    // likelihood contribution for the outcome
    if (y_obs_ci[i] == 0) {
      // observed outcome: PDF
      y_obs_c[i] ~ gamma(shape, shape ./ mu[i]);
    } else if (y_obs_ci[i] == 1) {
      // left-censored outcome: integrate out with CDF
      target += gamma_lcdf(y_obs_c[i] | shape, shape ./ mu[i]);
    }
  }
}

generated quantities {
  array[N_val] real<lower=0> x_val_imp; // imputed predictor
  array[N_val] real<lower=0> y_new_rep; // posterior predictive draws

  for (i in 1:N_val) {
    if (x_obs_ci_val[i] == 0) {
      // Uncensored predictor: use observed value directly
      x_val_imp[i] = x_obs_c_val[i];
    } else {
      // Censored predictor (x <= LoD): rejection sampling
      // sample from Gamma(shape_x, rate_x) until we get a value below the LoD
      real x_draw = gamma_rng(shape_x, rate_x);
      while (x_draw > x_obs_c_val[i]) {
        x_draw = gamma_rng(shape_x, rate_x);
      }
      x_val_imp[i] = x_draw;
    }

    // expected log-gamma mean for validation point
    real mu_val = exp(beta0 + beta1 * log(x_val_imp[i]));

    // posterior predictive draw
    y_new_rep[i] = gamma_rng(shape, shape / mu_val);
  }
}
