# simulate-data.R


#' sim_data
#'
#' @description
#' Function to simulate bivariate dataset with known properties on measurement error. The two variables are meant to represent the same soil property measured with two different methods.
#' @param N Sample size to generate
#' @param alpha Intercept of the linear relation between latent X and Y (default 0).
#' @param beta Slope of the linear relation between latent X and Y (default 1).
#' @param mu_x Mean of latent X distribution (default 0).
#' @param sigma2_x Variance of latent X distribution (default 1).
#' @param ratio_varmey_varmex Ratio of measurement error variance in Y vs. measurement error variance in X (default 1 as in Deming regression).
#' @param ratio_varmex_varx Ratio of measurement error variance in X vs. variance in latent X.
#' @param tails Character, should be one of "normal" (normally distributed measurement errors), "t_df3" (t-distribution with 3 dof).
#'
#' @export
sim_data <- function(
    N,
    ratio_varmex_varx,
    tails,
    alpha = 0,
    beta = 1,
    mu_x = 0,
    sigma2_x = 1,
    ratio_varmey_varmex = 1
) {

  # latent predictor
  x_true <- rnorm(N, mu_x, sqrt(sigma2_x))
  # observed predictor
  sigma2_mex <- ratio_varmex_varx * sigma2_x
  x_obs <- switch(
    tails,
    normal = x_true + rnorm(N, 0, sqrt(sigma2_mex)),
    t_df3  = x_true + extraDistr::rlst(N, 3, 0, sqrt(sigma2_mex))
  )

  # latent outcome
  y_true <- alpha + beta * x_true
  # observed outcome
  sigma2 <- ratio_varmey_varmex * sigma2_mex
  y_obs <- switch(
    tails,
    normal = y_true + rnorm(N, 0, sqrt(sigma2)),
    t_df3  = y_true + extraDistr::rlst(N, 3, 0, sqrt(sigma2))
  )

  # return data.frame with observed and latent variables
  df <- data.frame(x_true, y_true, x_obs, y_obs)
  return(df)
}
