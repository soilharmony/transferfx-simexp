# simulate-data.R

# sim_data
sim_data <- function(
    N,
    ratio_sdmex_sigmax,
    ratio_sdmey_sdmex,
    tails,
    alpha = 0,
    beta = 1,
    mu_x = 0,
    sigma_x = 1
) {
  
  # latent predictor
  x_true <- rnorm(N, mu_x, sigma_x)
  # observed predictor
  sdmex <- ratio_sdmex_sigmax * sigma_x
  x_obs <- switch(
    tails,
    normal = x_true + rnorm(N, 0, sdmex),
    tdf3   = x_true + extraDistr::rlst(N, 3, 0, sdmex)
  )
  
  # latent outcome
  y_true <- alpha + beta * x_true
  # observed outcome
  sdmey <- ratio_sdmey_sdmex * sdmex
  y_obs <- switch(
    tails,
    normal = y_true + rnorm(N, 0, sdmey),
    tdf3   = y_true + extraDistr::rlst(N, 3, 0, sdmey)
  )
  
  # list for STAN models
  df_stan <- list( 
    N         = N, 
    y_obs     = y_obs,
    x_obs     = x_obs,
    sigma_mex = sdmex # only for use in model with known SD of meas.err
  )
  
  # additionally create an independent validation dataset (N = 1000)
  Nval <- 1000
  x_true_val <- rnorm(Nval, mu_x, sigma_x)
  y_true_val <- alpha + beta * x_true_val
  x_obs_val <- switch(
    tails,
    normal = x_true_val + rnorm(Nval, 0, sdmex),
    tdf3   = x_true_val + extraDistr::rlst(Nval, 3, 0, sdmex)
  )
  y_obs_val <- switch(
    tails,
    normal = y_true_val + rnorm(Nval, 0, sdmey),
    tdf3   = y_true_val + extraDistr::rlst(Nval, 3, 0, sdmey)
  )
  data_val <- data.frame(
    uniqueid = 1:Nval, # observation ID, useful for joining
    y_true   = y_true_val,
    x_obs    = x_obs_val
  )
  df_stan$validation_data <- data_val
  
  return(df_stan)
}

