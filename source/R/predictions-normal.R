# predictions-normal.R
# set of R functions for predictions in case of normal DGM



# -------------------------------------------------------------------------
# linear regression model -------------------------------------------------

#' @title predict_linreg
#' @param draws_df data.frame with posterior draws
#' @param new_data data.frame with new observations to make predictions for
#' @param level Uncertainty level for the prediction interval, default = .95
predict_linreg <- function(draws_df, new_data) {
  stopifnot(# should be the same!
    length(new_data) == length(unique(draws_df$.rep))
  )
  
  # keep the necessary model parameters and nest them by .rep
  draws_list <- draws_df %>% 
    select(.rep, b_Intercept, b_x_obs, sigma) %>%
    nest(.by = .rep, .key = "draws") %>%
    mutate(
      # extract the separate validation datasets and add them
      validation_data = lapply(new_data, function(x) x$validation_data)
    )
  
  # apply function to make predictions
  preds <- draws_list %>%
    mutate(preds = purrr::map2(draws, validation_data, 
                               predx_linreg_matrix)) %>%
    select(.rep, preds) %>%
    unnest(preds)
  return(preds)
}

predx_linreg_matrix <- function(draws, newdata, level = .95) {
  
  N <- nrow(newdata)
  D <- nrow(draws)
  
  # 1. Matrix multiplication for the linear predictor 'eta'
  X <- cbind(1, newdata$x_obs)
  B <- cbind(draws$b_Intercept, draws$b_x_obs)
  eta_mat <- X %*% t(B) 
  
  # 2. Generate posterior predictive distributions (ydraw)
  noise <- rnorm(N * D, mean = 0, sd = rep(draws$sigma, each = N))
  ydraw_mat <- eta_mat + matrix(noise, nrow = N, ncol = D)
  
  # 3. Calculate EXACT Pointwise LPPD for the validation data
  log_lik_mat <- matrix(NA_real_, nrow = N, ncol = D)
  for (d in 1:D) {
    # Evaluate density of the TRUE unseen observation (y_obs) 
    # against the posterior
    log_lik_mat[, d] <- dnorm(
      x    = newdata$y_obs, 
      mean = eta_mat[, d], 
      sd   = draws$sigma[d], 
      log  = TRUE
    )
  }
  # Log-sum-exp trick to average probabilities safely
  lppd_pointwise <- matrixStats::rowLogSumExps(log_lik_mat) - log(D)

  # 4. Calculate point predictions and intervals
  yhat <- rowMeans(eta_mat)
  quants <- matrixStats::rowQuantiles(
    ydraw_mat, 
    probs = c((1 - level) / 2, 1 - (1 - level) / 2)
  )
  
  # 5. Continuous Ranked Probability Scores
  # => compare each y_obs in the validation dataset against the empirical CDF
  #    of the posterior predictive distribution
  crps <- scoringRules::crps_sample(
    dat = ydraw_mat,
    y   = newdata$y_obs
  )
  # sanity check: plot(lppd_pointwise, crps) => strong negative correlation
  
  # 6. Attach back to the original data
  newdata$yhat    <- yhat
  newdata$yhat_ll <- quants[, 1]
  newdata$yhat_ul <- quants[, 2]
  newdata$lppd    <- lppd_pointwise
  newdata$crps    <- crps

  newdata %>% 
    select(uniqueid, y_true, y_obs, yhat, yhat_ll, yhat_ul, lppd, crps)
}



# -------------------------------------------------------------------------
# EIV model (same for both types of EIV) ----------------------------------

#' @title predict_eivreg
#' @param draws_df data.frame with posterior draws
#' @param new_data data.frame with new observations to make predictions for
predict_eivreg <- function(draws_df, new_data) {
  stopifnot(# should be the same!
    length(new_data) == length(unique(draws_df$.rep))
  )
  
  # keep the necessary model parameters and nest them by .rep
  draws_list <- draws_df %>% 
    select(.rep, b_Intercept, b_x_obs,
           tilde_v, inv_var_x, mu_x, inv_var_mex, sd_yobs) %>%
    nest(.by = .rep, .key = "draws") %>%
    mutate(
      # extract the separate validation datasets and add them
      validation_data = lapply(new_data, function(x) x$validation_data)
    )
  
  # apply function to make predictions
  preds <- draws_list %>%
    mutate(preds = purrr::map2(draws, validation_data, 
                               predx_eivreg_matrix)) %>%
    select(.rep, preds) %>%
    unnest(preds)
  return(preds)
}

predx_eivreg_matrix <- function(draws, newdata, level = .95) {
  N <- nrow(newdata)
  D <- nrow(draws)
  
  # 1. Distribute terms to isolate the intercept and slope components
  eff_intercept <- draws$b_Intercept + 
    (draws$b_x_obs * draws$tilde_v * draws$inv_var_x * draws$mu_x)
  eff_slope     <- draws$b_x_obs * draws$tilde_v * draws$inv_var_mex
  
  # 2. Matrix multiplication for the linear predictor 'eta'
  X <- cbind(1, newdata$x_obs)
  B <- cbind(eff_intercept, eff_slope)
  eta_mat <- X %*% t(B) 
  
  # 3. Generate posterior predictive distributions (ydraw)
  noise <- rnorm(N * D, mean = 0, sd = rep(draws$sd_yobs, each = N))
  ydraw_mat <- eta_mat + matrix(noise, nrow = N, ncol = D)
  
  # 4. Calculate EXACT Pointwise LPPD for the validation data
  log_lik_mat <- matrix(NA_real_, nrow = N, ncol = D)
  for (d in 1:D) {
    log_lik_mat[, d] <- dnorm(
      x    = newdata$y_obs, 
      mean = eta_mat[, d], 
      sd   = draws$sd_yobs[d], 
      log  = TRUE
    )
  }
  lppd_pointwise <- matrixStats::rowLogSumExps(log_lik_mat) - log(D)
  
  # 5. Calculate point predictions and intervals
  yhat <- rowMeans(eta_mat)
  quants <- matrixStats::rowQuantiles(
    ydraw_mat, 
    probs = c((1 - level) / 2, 1 - (1 - level) / 2)
  )
  
  # 6. Continuous Ranked Probability Scores
  # => compare each y_obs in the validation dataset against the empirical CDF
  #    of the posterior predictive distribution
  crps <- scoringRules::crps_sample(
    dat = ydraw_mat,
    y   = newdata$y_obs
  )
  
  # 7. Attach back to the original data
  newdata$yhat    <- yhat
  newdata$yhat_ll <- quants[, 1]
  newdata$yhat_ul <- quants[, 2]
  newdata$lppd    <- lppd_pointwise
  newdata$crps    <- crps
  
  newdata %>% 
    select(uniqueid, y_true, y_obs, yhat, yhat_ll, yhat_ul, lppd, crps)
}


# -------------------------------------------------------------------------
# EIV model (specific for known SD meas.err.) -----------------------------

#' @title predict_eivreg_knownsd
#' @param draws_df data.frame with posterior draws
#' @param new_data data.frame with new observations to make predictions for
predict_eivreg_knownsd <- function(draws_df, new_data) {
  stopifnot(# should be the same!
    length(new_data) == length(unique(draws_df$.rep))
  )
  
  # keep the necessary model parameters and nest them by .rep
  draws_list <- draws_df %>% 
    select(.rep, b_Intercept, b_x_obs, inv_var_x, mu_x, sigma) %>%
    nest(.by = .rep, .key = "draws") %>%
    mutate(
      # extract the separate validation datasets and add them
      validation_data = lapply(new_data, function(x) x$validation_data),
      # extract the known SD meas.err for the validation data
      validation_data_sdme = lapply(
        new_data, function(x) x$validation_data_sdmeaserr
      )
    )
  
  # apply function to make predictions
  preds <- draws_list %>%
    mutate(
      preds = purrr::pmap(list(draws, validation_data, validation_data_sdme),
                          predx_eivreg_knownsd_matrix)
    ) %>%
    select(.rep, preds) %>%
    unnest(preds)
  return(preds)
}

predx_eivreg_knownsd_matrix <- function(draws, newdata, newdatasd, level = .95) {
  N <- nrow(newdata)
  D <- nrow(draws)
  
  # 1. Compute dynamic components based on the known validation SD
  inv_var_mex <- 1 / (newdatasd^2)
  tilde_v     <- 1 / (draws$inv_var_x + inv_var_mex)
  sd_yobs     <- sqrt(draws$sigma^2 + (draws$b_x_obs^2) * tilde_v)
  
  # 2. Distribute terms to isolate the effective intercept and slope
  eff_intercept <- draws$b_Intercept + 
    (draws$b_x_obs * tilde_v * draws$inv_var_x * draws$mu_x)
  eff_slope     <- draws$b_x_obs * tilde_v * inv_var_mex
  
  # 3. Matrix multiplication for the linear predictor 'eta'
  X <- cbind(1, newdata$x_obs)
  B <- cbind(eff_intercept, eff_slope)
  eta_mat <- X %*% t(B) 
  
  # 4. Generate posterior predictive distributions (ydraw)
  noise <- rnorm(N * D, mean = 0, sd = rep(sd_yobs, each = N))
  ydraw_mat <- eta_mat + matrix(noise, nrow = N, ncol = D)
  
  # 5. Calculate EXACT Pointwise LPPD for the validation data
  log_lik_mat <- matrix(NA_real_, nrow = N, ncol = D)
  for (d in 1:D) {
    log_lik_mat[, d] <- dnorm(
      x    = newdata$y_obs, 
      mean = eta_mat[, d], 
      sd   = sd_yobs[d], 
      log  = TRUE
    )
  }
  lppd_pointwise <- matrixStats::rowLogSumExps(log_lik_mat) - log(D)
  
  # 6. Calculate point predictions and intervals
  yhat <- rowMeans(eta_mat)
  quants <- matrixStats::rowQuantiles(
    ydraw_mat, 
    probs = c((1 - level) / 2, 1 - (1 - level) / 2)
  )
  
  # 7. Continuous Ranked Probability Scores
  # => compare each y_obs in the validation dataset against the empirical CDF
  #    of the posterior predictive distribution
  crps <- scoringRules::crps_sample(
    dat = ydraw_mat,
    y   = newdata$y_obs
  )
  
  # 8. Attach back to the original data
  newdata$yhat    <- yhat
  newdata$yhat_ll <- quants[, 1]
  newdata$yhat_ul <- quants[, 2]
  newdata$lppd    <- lppd_pointwise
  newdata$crps    <- crps
  
  newdata %>% 
    select(uniqueid, y_true, y_obs, yhat, yhat_ll, yhat_ul, lppd, crps)
}


