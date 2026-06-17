# predictions.R
# set of R functions for predictions



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
    mutate(preds = purrr::map2(draws, validation_data, predx_linreg)) %>%
    select(.rep, preds) %>%
    unnest(preds)
  return(preds)
}

#' @title predx_linreg
#' @description
#' Actual prediction function for the linear models
#' @param draws ....
#' @param newdata ....
#' @param level ....
predx_linreg <- function(draws, newdata, level = .95) {
  # linear predictor
  eta <- expand_grid(draws, newdata) %>%
    mutate(eta = b_Intercept + b_x_obs * x_obs)
  
  # posterior point prediction & prediction interval
  yhat <- eta %>%
    mutate(ydraw = rnorm(n(), eta, sigma)) %>%
    summarise(
      .by     = uniqueid, 
      yhat    = mean(eta),
      yhat_ll = quantile(ydraw, (1 - level) / 2),
      yhat_ul = quantile(ydraw, 1 - (1 - level) / 2)
    )
  
  newdata %>% 
    select(uniqueid, y_true, y_obs) %>%
    left_join(yhat, by = "uniqueid")
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
    mutate(preds = purrr::map2(draws, validation_data, predx_eivreg)) %>%
    select(.rep, preds) %>%
    unnest(preds)
  return(preds)
}

#' @title predx_eivreg
#' @description
#' Actual prediction function for the EIV models
#' @param draws ....
#' @param newdata ....
#' @param level ....
predx_eivreg <- function(draws, newdata, level = .95) {
  # linear predictor
  eta <- expand_grid(draws, newdata) %>%
    mutate(
      tilde_mu = tilde_v * (inv_var_x * mu_x + inv_var_mex * x_obs),
      eta      = b_Intercept + b_x_obs * tilde_mu
    )
  
  # posterior point prediction & prediction interval
  yhat <- eta %>%
    mutate(ydraw = rnorm(n(), eta, sd_yobs)) %>%
    summarise(
      .by     = uniqueid, 
      yhat    = mean(eta),
      yhat_ll = quantile(ydraw, (1 - level) / 2),
      yhat_ul = quantile(ydraw, 1 - (1 - level) / 2)
    )
  
  newdata %>% 
    select(uniqueid, y_true, y_obs) %>%
    left_join(yhat, by = "uniqueid")
}
