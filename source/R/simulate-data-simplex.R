# simulate-data-simplex.R

#' sim_data_simplex
#' 
#' @description
#' Function to simulate data for a TF on the 3-D simplex (ternary plot). Particle size distributions are the candidate soil descriptors: clay + silt + sand = 1.
#' 
#' @details
#' Data are simulated in the two-dimensional real space of isometric log-ratio components (ILR) where ternary simplex data behave like bivariate normal variables. The values are returned as both ILR-components as back-transformed particle size distributions that sum to 100%.
#' 
#' @param sample_size sample size for the training (calibration) dataset
#' @param mu_ilr Vector of means in the ILR space. The default values for mu_ilr and cov_ilr are obtained from a fit to Eurofins data.
#' @param cov_ilr Variance-covariance matrix in the ILR-space
#' @param alpha_ilr The latent reference (y) and non-reference methods (x) are related through the same linear regression model on the ILR-scale:
#'  - `ilr-y1 = alpha_ilr + beta_ilr * ilr-x1 + err`
#'  - `ilr-y2 = alpha_ilr + beta_ilr * ilr-x2 + err`
#' @param beta_ilr see `alpha_ilr`
#' @param sigma_ilr Equation error for the TF between the latent reference and non-reference methods (`err` in the equation above). This part of the error is not explained by measurement error. We take the same value for both ILR-components.
#' @param ratio_sdmex_sdx Ratio between the SD of the measurement error of the non-reference method and the sampling SD of the non-reference method (square-root of the diagonal of `cov_ilr`).
#' @param ratio_sdmey_sdmex Ratio between the SD of the measurement error between the reference method and non-reference method.
#' @param corr_mexy Correlation coefficient between the measurement error in the reference method and non-reference method.
#' @param ratio_sdmeval_sdmecal Ratio of the SD's of the measurement error between validation and calibration datasets.
#' 
sim_data_simplex <- function(
  sample_size           = 100,
  mu_ilr                = c(.5, .25),
  cov_ilr               = matrix(c(.16, -.14, -.14, 1.34), 
                                 byrow = T, nrow = 2),
  alpha_ilr             = 0,  
  beta_ilr              = 1,  
  sigma_ilr             = .1, 
  ratio_sdmex_sdx       = .05,
  ratio_sdmey_sdmex     = 1,
  corr_mexy             = 0,
  ratio_sdmeval_sdmecal = 1 
) {
  N <- sample_size
  
  # simulate latent values for the non-reference method (ILR-scale)
  ilr_x <- MASS::mvrnorm(N, mu_ilr, cov_ilr)
  
  # regression to latent values of the reference method (ILR-scale)
  ilr_y <- alpha_ilr + beta_ilr * ilr_x +
    matrix(rnorm(N * 2, 0, sigma_ilr), nrow = N, ncol = 2)
  
  # transform latent ILR to PSD (%)
  psd_x <- data.frame(compositions::ilrInv(ilr_x))
  colnames(psd_x) <- c("x_true_clay","x_true_silt","x_true_sand")
  psd_y <- data.frame(compositions::ilrInv(ilr_y))
  colnames(psd_y) <- c("y_true_clay","y_true_silt","y_true_sand")
  psd <- cbind(psd_x, psd_y)
  
  # add measurement noise: additive noise on ILR-scale
  sd_mex <- sqrt(diag(cov_ilr)) * ratio_sdmex_sdx
  sd_mey <- sd_mex * ratio_sdmey_sdmex
  eps_xy <- MASS::mvrnorm(
    n     = N, 
    mu    = rep(0, 4), 
    Sigma = matrix(
      c(sd_mex[1]^2, 0, corr_mexy * sd_mex[1] * sd_mey[1], 0,
        0, sd_mex[2]^2, 0, corr_mexy * sd_mex[2] * sd_mey[2],
        corr_mexy * sd_mex[1] * sd_mey[1], 0, sd_mey[1]^2, 0,
        0, corr_mexy * sd_mex[2] * sd_mey[2], 0, sd_mey[2]^2), 
      byrow = T, nrow = 4
    )
  )
  ilr_x_obs <- ilr_x + eps_xy[, 1:2]
  ilr_y_obs <- ilr_y + eps_xy[, 3:4]
  
  # transform observed to PSD (%)
  psd_x_obs <- data.frame(compositions::ilrInv(ilr_x_obs))
  colnames(psd_x_obs) <- c("x_obs_clay","x_obs_silt","x_obs_sand")
  psd_y_obs <- data.frame(compositions::ilrInv(ilr_y_obs))
  colnames(psd_y_obs) <- c("y_obs_clay","y_obs_silt","y_obs_sand")
  psd_obs <- cbind(psd_x_obs, psd_y_obs)
  
  #####
  # same procedure for validation data
  N_new     <- 1000
  ilr_x_new <- MASS::mvrnorm(N_new, mu_ilr, cov_ilr)
  ilr_y_new <- alpha_ilr + beta_ilr * ilr_x_new +
    matrix(rnorm(N_new * 2, 0, sigma_ilr), nrow = N_new, ncol = 2)
  psd_x_new           <- data.frame(compositions::ilrInv(ilr_x_new))
  colnames(psd_x_new) <- c("x_true_clay","x_true_silt","x_true_sand")
  psd_y_new           <- data.frame(compositions::ilrInv(ilr_y_new))
  colnames(psd_y_new) <- c("y_true_clay","y_true_silt","y_true_sand")
  psd_new             <- cbind(psd_x_new, psd_y_new)
  sd_mex_new <- sd_mex * ratio_sdmeval_sdmecal
  sd_mey_new <- sd_mey * ratio_sdmeval_sdmecal
  eps_xy_new <- MASS::mvrnorm(
    n     = N_new, 
    mu    = rep(0, 4), 
    Sigma = matrix(
      c(sd_mex_new[1]^2, 0, corr_mexy * sd_mex_new[1] * sd_mey_new[1], 0,
        0, sd_mex_new[2]^2, 0, corr_mexy * sd_mex_new[2] * sd_mey_new[2],
        corr_mexy * sd_mex_new[1] * sd_mey_new[1], 0, sd_mey_new[1]^2, 0,
        0, corr_mexy * sd_mex_new[2] * sd_mey_new[2], 0, sd_mey_new[2]^2), 
      byrow = T, nrow = 4
    )
  )
  ilr_x_obs_new <- ilr_x_new + eps_xy_new[, 1:2]
  ilr_y_obs_new <- ilr_y_new + eps_xy_new[, 3:4]
  psd_x_obs_new           <- data.frame(compositions::ilrInv(ilr_x_obs_new))
  colnames(psd_x_obs_new) <- c("x_obs_clay","x_obs_silt","x_obs_sand")
  psd_y_obs_new           <- data.frame(compositions::ilrInv(ilr_y_obs_new))
  colnames(psd_y_obs_new) <- c("y_obs_clay","y_obs_silt","y_obs_sand")
  psd_obs_new             <- cbind(psd_x_obs_new, psd_y_obs_new)
  
  # output for use in STAN models
  # not each element in the list is necessary for each model, STAN will ignore
  # unused elements by default
  df_stan <- list(
    K = 3, # 3-dimensional simplex
    
    #### calibration data
    N = N, 
    
    # mv-linreg model on ILR-scale
    ilr_x_obs = ilr_x_obs, # predictor variables
    ilr_y_obs = ilr_y_obs, # outcome variables
    
    # for mv-linreg with known covariance of the measurement error
    Sigma_mex = diag(sd_mex^2),
    
    # Dirichlet model on simplex
    psd_x_obs = psd_x_obs, # predictor variables
    psd_y_obs = psd_y_obs, # outcome variables
    
    #### validation data
    N_new         = N_new,
    ilr_x_obs_new = ilr_x_obs_new,
    ilr_y_obs_new = ilr_y_obs_new,
    psd_new       = cbind(id = 1:N_new, psd_new, psd_obs_new),
    Sigma_mex_new = diag(sd_mex_new^2)
  )
  return(df_stan)
}


