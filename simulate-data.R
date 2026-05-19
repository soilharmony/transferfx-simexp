# simulate-data.R


#' sim_data
#'
#' @description
#' Function to simulate bivariate dataset with known properties on measurement error. The two variables are meant to represent the same soil property measured with two different methods.
#' @param N Sample size to generate
#' @param alpha Intercept of the linear relation between latent X and Y (default 0).
#' @param beta Slope of the linear relation between latent X and Y (default 1).
#' @param mu_x Mean of latent X distribution (default 0).
#' @param sigma_x SD of latent X distribution (default 1).
#' @param ratio_sdmey_sdmex Ratio between SD of measurement error in Y and SD of measurement error in X (default 1 as in Deming regression).
#' @param ratio_sdmex_sigmax Ratio between SD of measurement error in X and SD of latent X.
#' @param tails Character, should be one of "normal" (normally distributed measurement errors), "t_df3" (t-distribution with 3 dof).
#'
#' @export
sim_data <- function(
    N,
    ratio_sdmex_sigmax,
    tails,
    alpha = 0,
    beta = 1,
    mu_x = 0,
    sigma_x = 1,
    ratio_sdmey_sdmex = 1
) {

  # latent predictor
  x_true <- rnorm(N, mu_x, sigma_x)
  # observed predictor
  sdmex <- ratio_sdmex_sigmax * sigma_x
  x_obs <- switch(
    tails,
    normal = x_true + rnorm(N, 0, sdmex),
    t_df3  = x_true + extraDistr::rlst(N, 3, 0, sdmex)
  )

  # latent outcome
  y_true <- alpha + beta * x_true
  # observed outcome
  sdmey <- ratio_sdmey_sdmex * sdmex
  y_obs <- switch(
    tails,
    normal = y_true + rnorm(N, 0, sdmey),
    t_df3  = y_true + extraDistr::rlst(N, 3, 0, sdmey)
  )

  # return data.frame with observed and latent variables
  df <- data.frame(x_true, y_true, x_obs, y_obs)
  return(df)
}
