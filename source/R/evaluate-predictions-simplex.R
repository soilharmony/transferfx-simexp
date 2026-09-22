# evaluate-predictions-simplex.R
#
# set of R functions to evaluate the predictions in experiments with 
# simplex-distributed variables


#' summarise_predictions
#' summarise predictions and bind with validation data
#' @param mcmc posterior draws
#' @param data list of per-dataset Stan data lists
summarise_predictions <- function(mcmc, data) {
  #browser()
  
  draws_new_obs <- mcmc %>%
    select(.dataset_id, .rep, contains("ilr_y_obs_new_rep"))
  
  spc <- data.frame(
    .name = colnames(draws_new_obs)[3:ncol(draws_new_obs)]
  ) %>%
    mutate(
      .value = case_when(grepl(",1]", .name) ~ "X1",
                         grepl(",2]", .name) ~ "X2"),
      id = stringr::str_extract(.name, "(?<=\\[)\\d+")
    )
  
  draws_new_obs_long <- draws_new_obs %>%
    pivot_longer_spec(spc) %>%
    nest(.by = c(.dataset_id, .rep, id), .key = "ilr_pred")
  
  validation_data <- data.table::rbindlist(
    lapply(data, get_validation_data)
  )

  # Chi-square critical value for 95% region, 2 df
  crit <- sqrt( qchisq(0.95, df = 2) )
  # make a unit circle as a basis to construct a 95% prediction ellipse
  theta  <- seq(0, 2*pi, length.out = 200)
  circle <- cbind(cos(theta), sin(theta))
  
  # calculate prediction errors in the ILR-space
  pred_summary <- validation_data %>%
    left_join(draws_new_obs_long, by = c(".dataset_id","id")) %>%
    mutate(
      err = map2(ilr_obs, ilr_pred, .f = ilr_err, 
                 crit=crit, circle=circle)
    ) %>%
    unnest(err) %>%
    select(.dataset_id, .rep, id, adist, in_ellipse)
}


#' get_validation_data
#' helper function for summarise_predictions()
get_validation_data <- function(data) {
  data.frame(
    .dataset_id = rep(data$.dataset_id, data$N_new),
    id          = as.character(data$psd_new$id),
    y_obs_clay  = data$psd_new$y_obs_clay,
    y_obs_silt  = data$psd_new$y_obs_silt,
    y_obs_sand  = data$psd_new$y_obs_sand
  ) %>%
    nest(.by = c(.dataset_id, id), .key = "psd_obs") %>%
    mutate(ilr_obs = map(psd_obs, compositions::ilr))
}

#' ilr_err
#' helper function for summarise_predictions()
ilr_err <- function(obs, pred, crit, circle) {
  #browser()
  # Aitchinson distance between obs & point estimate (bias):
  est   <- colMeans(pred) # point-estimate in ILR-space
  adist <- compositions::norm(obs - est)
  # prediction coverage:
  err         <- sweep(pred, 2, est, "-")
  err_mean    <- colMeans(err)
  err_sigma   <- cov(err)
  L           <- chol(err_sigma) # scale/rotate unit circle into the ellipse
  ellipse_ilr <- crit * (circle %*% L)
  ellipse_ilr <- sweep(ellipse_ilr, 2, est, "+") # recenter on point estimate
  in_ellipse  <- between(obs[1], min(ellipse_ilr[,1]), max(ellipse_ilr[,1])) &
    between(obs[2], min(ellipse_ilr[,2]), max(ellipse_ilr[,2]))
  
  return(data.frame(adist = adist, in_ellipse = in_ellipse))
}


#' eval_preds_simplex
#' evaluate prediction models for simplex-distributed variables
#' @param ... set of models to evaluate, passed as different arguments
eval_preds_simplex <- function(...) {
  #browser()
  preds  <- list(...)
  names(preds) <- stringr::str_split_i(
    as.character(as.list(substitute(list(...)))[-1]),
    "_", 2
  )
  for (i in 1:length(preds)) {
    preds[[i]] <- preds[[i]] %>%
      mutate(model = names(preds)[i])
  }
  data.table::rbindlist(
    lapply(preds, val_metrics_simplex)
  )
}

#' val_metrics_simplex
#' helper function for `eval_preds_simplex()`: 
#' calculation of the validation metrics
val_metrics_simplex <- function(df) {
  df %>%
    summarise(
      .by = c(model, .rep),
      MAD  = mean(adist),     # mean Aitchinson distance
      PECP = mean(in_ellipse) # prediction ellipse coverage probability
    ) 
}

