# simulate-data-leftcens.R

#' sim_data_leftcens
#' 
#' @description
#' Function to simulate data for a TF on gamma distributed left-censored variables (outcome and predictor can be subject to left-censoring).
#' 
#' @param sample_size sample size for the training (calibration) dataset
#' 
sim_data_leftcens <- function(
    sample_size          = 200,
    mu_x                 = 1,      
    cv_x                 = .5, 
    alpha                = 0,
    beta                 = 1,
    cv_y                 = 0.05,
    ratio_cvmex_cvx      = 0.05,
    ratio_mey_mex        = 1,
    corr_error           = 0,
    ratio_cvme_val_train = 1,
    quantile_lodx        = 0.1,
    ratio_lodyx          = 1
) {
  N <- sample_size
  
  # latent predictor ~ Gamma()
  # transform (mu, cv) -> (shape, rate)
  x_true <- rgamma(N, shape = cv_x^(-2), rate = cv_x^(-2) / mu_x)
  
  # latent outcome: Gamma-GLM with log-link
  mu_y   <- exp(alpha + beta * log(x_true))
  y_true <- rgamma(N, shape = cv_y^(-2), rate = cv_y^(-2) / mu_y)
  
  # multiplicative measurement error on x and y
  cv_mex <- cv_x * ratio_cvmex_cvx
  cv_mey <- cv_mex * ratio_mey_mex
  # Gaussian copula -> correlated uniforms -> gamma marginals, mean 1
  # bivariate standard-normal with correlation for use in copula's
  Sigma <- matrix(c(1, corr_error, corr_error, 1), 2, 2)
  Z     <- MASS::mvrnorm(N, mu = c(0, 0), Sigma = Sigma)
  U     <- pnorm(Z)
  eps1  <- qgamma(U[, 1], shape = cv_mex^(-2), rate = cv_mex^(-2))
  eps2  <- qgamma(U[, 2], shape = cv_mey^(-2), rate = cv_mey^(-2))
  x_obs <- x_true * eps1
  y_obs <- y_true * eps2
  
  # censoring applies to the observed variables, not to the latent variables
  lodx <- qgamma(quantile_lodx, shape = cv_x^(-2), rate = cv_x^(-2) / mu_x)
  lody <- lodx * ratio_lodyx
  x_obs_c                  <- x_obs
  y_obs_c                  <- y_obs
  x_obs_c[x_obs_c <= lodx] <- lodx
  y_obs_c[y_obs_c <= lody] <- lody
  x_obs_ci                 <- ifelse(x_obs <= lodx, 1L, 0L)
  y_obs_ci                 <- ifelse(y_obs <= lody, 1L, 0L)
  
  
  # now the same for the validation dataset (N_val = 1000)
  Nval       <- 1000
  cv_mex_val <- cv_mex * ratio_cvme_val_train
  cv_mey_val <- cv_mey * ratio_cvme_val_train
  x_true_val <- rgamma(Nval, shape = cv_x^(-2), rate = cv_x^(-2) / mu_x)
  mu_y_val   <- exp(alpha + beta * log(x_true_val))
  y_true_val <- rgamma(Nval, shape = cv_y^(-2), rate = cv_y^(-2) / mu_y_val)
  Zval       <- MASS::mvrnorm(Nval, mu = c(0, 0), Sigma = Sigma)
  Uval       <- pnorm(Zval)
  eps1_val   <- qgamma(Uval[, 1], shape=cv_mex_val^(-2), rate=cv_mex_val^(-2))
  eps2_val   <- qgamma(Uval[, 2], shape=cv_mey_val^(-2), rate=cv_mey_val^(-2))
  x_obs_val  <- x_true_val * eps1_val
  y_obs_val  <- y_true_val * eps2_val
  x_obs_c_val                     <- x_obs_val
  y_obs_c_val                     <- y_obs_val
  x_obs_c_val[x_obs_c_val < lodx] <- lodx
  y_obs_c_val[y_obs_c_val < lody] <- lody
  x_obs_ci_val                    <- ifelse(x_obs_val <= lodx, 1L, 0L)
  y_obs_ci_val                    <- ifelse(y_obs_val <= lody, 1L, 0L)
  
  
  #### return STAN list ###
  stan_list <- list(
    # calibration data
    N        = N,
    lodx     = lodx, # assume same LoD for validation data
    lody     = lody, # assume same LoD for validation data
    Nobsy    = sum(y_obs > lody),
    Nceny    = sum(y_obs <= lody),
    Nobsx    = sum(x_obs > lodx),
    Ncenx    = sum(x_obs <= lodx),
    x_obs_c  = x_obs_c,
    y_obs_c  = y_obs_c,
    x_obs_ci = x_obs_ci,
    y_obs_ci = y_obs_ci,
    cv_mex   = cv_mex, # for the model with known CV of the measurement error
    x_true   = x_true, # only for visualizations, not used in STAN
    y_true   = y_true, # only for visualizations, not used in STAN
    x_obs    = x_obs,  # only for visualizations, not used in STAN
    y_obs    = y_obs,  # only for visualizations, not used in STAN
    
    # validation data
    N_val        = Nval,
    x_obs_c_val  = x_obs_c_val,
    y_obs_c_val  = y_obs_c_val,
    x_obs_ci_val = x_obs_ci_val,
    y_obs_ci_val = y_obs_ci_val,
    cv_mex_val   = cv_mex_val
  )
  return(stan_list)
}
