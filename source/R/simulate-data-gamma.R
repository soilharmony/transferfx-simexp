# simulate-data-gamma.R


#' sim_data_gamma
#' 
#' DGM for gamma-distributed soil variables (eg. ECEC). Structured as follows:
#' `x_true ~ Gamma(mean = mu_x, CV = cv_x)`
#' `mu_y   = exp(alpha + beta * log(x_true))`
#' `y_true ~ Gamma(mean = mu_y, CV = cv_y)`
#' `x_obs` = `x_true * eps_x`
#' `y_obs  = y_true * eps_y`
#' 
#' @param N sample size
#' @param mu_x mean of the predictor variable (= shape/rate)
#' @param cv_x coefficient of variation of the predictor variable (= 1 / sqrt(shape))
#' @param alpha intercept on log-scale
#' @param beta slope on log-scale
#' @param cv_y structural noise on log-scale
#' @param cv_mex CV of the multiplicative measurement error (x)
#' @param ratio_mey_mex ratio between the CV of the meas.err: cv_mey / cv_mex
#' @param corr_error correlation between x- and y- measurement errors
sim_data_gamma <- function(
    sample_size          = 200,
    mu_x                 = 1,      
    cv_x                 = .5, 
    alpha                = 0,
    beta                 = 1,
    cv_y                 = 0.05,
    ratio_cvmex_cvx      = 0.05,
    ratio_mey_mex        = 1,
    corr_error           = 0,
    ratio_cvme_val_train = 1
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
  # solution with copula's with help from Claude-AI:
  # Gaussian copula -> correlated uniforms -> gamma marginals, mean 1
  # bivariate standard-normal with correlation for use in copula's
  Sigma <- matrix(c(1, corr_error, corr_error, 1), 2, 2)
  Z     <- MASS::mvrnorm(N, mu = c(0, 0), Sigma = Sigma)
  U     <- pnorm(Z)
  eps1  <- qgamma(U[, 1], shape = cv_mex^(-2), rate = cv_mex^(-2))
  eps2  <- qgamma(U[, 2], shape = cv_mey^(-2), rate = cv_mey^(-2))
  x_obs <- x_true * eps1
  y_obs <- y_true * eps2
  
  # additionally create an independent validation dataset (N = 1000)
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
  
  
  data_val <- data.frame(
    uniqueid = 1:Nval, # observation ID, useful for joining
    y_true   = y_true_val,
    y_obs    = y_obs_val,
    x_true   = x_true_val,
    x_obs    = x_obs_val
  )
  
  # list for use in STAN models
  df_stan <- list(
    N      = N,
    x_true = x_true, # not used by STAN
    y_true = y_true, # not used by STAN
    x_obs  = x_obs,
    y_obs  = y_obs,
    cv_mex = cv_mex, # only for use in EIV-model with known CV
    # additional data for prediction-on-the-fly for new observations
    N_new      = Nval,
    x_true_new = x_true_val, # not used by STAN
    y_true_new = y_true_val, # not used by STAN
    x_obs_new  = x_obs_val,
    y_obs_new  = y_obs_val,   # not used by STAN
    cv_mex_val = cv_mex_val,  # only for use in EIV-model with known CV
    # Appending the validation data frame to the output list
    validation_data = data_val # not used by STAN
  )
  
  return(df_stan)
}


#' #' sim_data_ECEC
#' #' wrapper around sim_data_gamma() for ECEC-like data
#' sim_data_ECEC <- function(N = 200, draws_params_hdi) {
#'   idx <- sample(1:nrow(draws_params_hdi), 1)
#'   df_stan <- sim_data_gamma(
#'     sample_size     = N,     # not a model parameter, we choose this
#'     mu_x            = draws_params_hdi$mu_x[idx], 
#'     cv_x            = draws_params_hdi$cv_x[idx],
#'     cv_y            = 1/sqrt(draws_params_hdi$shape[idx]),
#'     alpha           = draws_params_hdi$beta0[idx],
#'     beta            = draws_params_hdi$beta1[idx],
#'     ratio_cvmex_cvx = draws_params_hdi$ratio_cvmex_cvx[idx],
#'     ratio_mey_mex   = 1,     # not a model parameter, we choose this
#'     corr_error      = 0.5,   # not a model parameter, we choose this
#'     ratio_cvme_val_train = 1 # not a model parameter, we choose this
#'   )
#'   return(df_stan)
#' }
#' 

