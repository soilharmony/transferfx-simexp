# simulate-data.R

# sim_data
sim_data <- function(
    N = 100,
    ratio_sdmex_sigmax = .1,
    ratio_sdmey_sdmex = 1,
    corr_sdmey_sdmex = 0,
    tails = "normal",
    mu_x = 0,
    sigma_x = 1,
    alpha = 0,
    beta = 1,
    sigma_y_struct = .1
) {
  
  # latent predictor
  x_true <- rnorm(N, mu_x, sigma_x)
  # latent outcome
  y_true <- alpha + beta * x_true + rnorm(N, 0, sigma_y_struct)
  
  # observed predictor & outcome: add measurement error
  sdmex <- ratio_sdmex_sigmax * sigma_x
  sdmey <- ratio_sdmey_sdmex * sdmex
  varme <- matrix(
    c(sdmex^2, rep(sdmex * sdmey * corr_sdmey_sdmex, 2), sdmey^2),
    byrow = TRUE, nrow = 2
  )
  xy_obs <- switch(
    tails,
    normal = c(x_true, y_true) + MASS::mvrnorm(N, c(0, 0), varme),
    tdf3   = c(x_true, y_true) + mvtnorm::rmvt(N, 1/3*sqrt(varme), df = 3)
    # see the help file of mvtnorm::rmvt for why the 1/3 scaler is added
  )
  
  # list for STAN models
  df_stan <- list( 
    N         = N, 
    y_obs     = xy_obs[,2],
    x_obs     = xy_obs[,1],
    sigma_mex = sdmex # only for use in model with known SD of meas.err
  )
  
  # additionally create an independent validation dataset (N = 1000)
  Nval   <- 1000
  x_val  <- rnorm(Nval, mu_x, sigma_x)
  y_val  <- alpha + beta * x_true_val + rnorm(Nval, 0, sigma_y_struct)
  xy_val_obs <- switch(
    tails,
    normal = c(x_val, y_val) + MASS::mvrnorm(Nval, c(0, 0), varme),
    tdf3   = c(x_val, y_val) + mvtnorm::rmvt(Nval, 1/3*sqrt(varme), df = 3)
  )
  data_val <- data.frame(
    uniqueid = 1:Nval, # observation ID, useful for joining
    y_true   = y_val,
    y_obs    = xy_val_obs[,2],
    x_true   = x_val,
    x_obs    = xy_val_obs[,1]
  )
  df_stan$validation_data <- data_val
  
  return(df_stan)
}

