# evaluate-predictions-gamma.R
#
# set of R functions to evaluate the predictions in experiments with 
# gamma-distributed variables


#' summarise_predictions
#' summarise predictions and bind with validation data
#' @param mcmc posterior draws
#' @param data list of per-dataset Stan data lists
#' @param family likelihood family used for the exact lppd calculation --
#'   "gamma" for gammareg.stan and the gammareg_eiv_*.stan variants,
#'   "normal" for gamma_linreg.stan, "lognormal" for
#'   gamma_linreglogtrafo.stan. yhat/yhat_ll/yhat_ul stay Monte-Carlo based
#'   off `y_new_rep` as before, regardless of family -- only lppd needs to
#'   know the likelihood.
summarise_predictions <- function(
    mcmc, data, family = c("gamma", "normal", "lognormal")
) {
  family <- match.arg(family)
  #browser()
  preds <- mcmc %>%
    select(.rep, .dataset_id, contains("y_new_rep")) %>%
    nest(.by = c(.rep, .dataset_id), .key = "samples") %>%
    mutate(preds = map(samples, predx_summary)) %>%
    select(-samples) %>%
    unnest(preds) %>%
    mutate(.by = c(.rep, .dataset_id), uniqueid = 1:n())
  validation_data <- data.table::rbindlist(
    lapply(data, get_validation_data)
  ) %>% mutate(.by = .dataset_id, uniqueid = 1:n())
  lppd_crps <- compute_lppd_crps(mcmc, data, family = family)
  validation_data %>%
    left_join(preds, by = c(".dataset_id","uniqueid")) %>%
    left_join(lppd_crps,  by = c(".dataset_id","uniqueid")) %>%
    select(.rep, .dataset_id, uniqueid, 
           y_true_new, y_obs_new, yhat, yhat_ll, yhat_ul, lppd, crps)
}

#' predx_summary
#' helper function for summarise_predictions()
predx_summary <- function(samples) {
  samples_matrix <- t(as.matrix(samples))
  data.frame(
    yhat    = apply(samples_matrix, 1, mean),
    yhat_ll = apply(samples_matrix, 1, quantile, probs = .025),
    yhat_ul = apply(samples_matrix, 1, quantile, probs = .975)
  )
}

#' get_validation_data
#' helper function for summarise_predictions()
get_validation_data <- function(data) {
  data.frame(
    .dataset_id = rep(data$.dataset_id, data$N_new),
    x_true_new  = data$x_true_new,
    y_true_new  = data$y_true_new,
    x_obs_new   = data$x_obs_new,
    y_obs_new   = data$y_obs_new
  )
}


#' eval_preds_gamma
#' evaluate prediction models for gamma-distributed variables
#' @param ... set of models to evaluate, passed as different arguments
eval_preds_gamma <- function(...) {
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
    lapply(preds, val_metrics_gamma)
  )
}

#' val_metrics_gamma
#' helper function for `eval_preds_gamma()`: 
#' calculation of the validation metrics
val_metrics_gamma <- function(df) {
  df %>%
    summarise(
      .by = c(model, .rep),
      MAE  = mean(abs(yhat - y_obs_new)),
      RMSE = sqrt(mean((yhat - y_obs_new)^2)),
      MPE  = mean(yhat - y_obs_new),
      R2   = 1 - sum((y_obs_new - yhat)^2) / sum((y_obs_new-mean(y_obs_new))^2),
      SDPE = sd(yhat - y_obs_new),
      PICP = mean(between(y_obs_new, yhat_ll, yhat_ul)),
      # specific metrics for gamma: predictions should be strictly positive
      PNEG   = mean(yhat <= 0),
      PNEGll = mean(yhat_ll <= 0),
      ELPD    = sum(lppd),           # exact expected log predictive density
      ELPD_SE = sqrt(n() * var(lppd)),
      CRPS    = mean(crps),          # continuously ranked probability scores
      CRPS_SE = sd(crps) / sqrt(n())
    ) 
}

#' @title compare_models
#' @description
#' Paired ELPD/CRPS model comparison. This reproduces what loo::loo_compare()
#' computes internally (elpd_diff = sum of pointwise differences, se_diff =
#' sqrt(N * var(pointwise differences))) -- but pairs models on the SAME
#' validation observation (via `uniqueid`) rather than requiring a `loo`
#' S3 object, since the pointwise `lppd` column from predx_..._matrix()
#' already contains everything the comparison needs. Pairing matters here:
#' it gives a tighter, more appropriate SE than treating each model's ELPD
#' as independent, since both models are evaluated on identical held-out
#' points.
#' @param ... set of models to compare, passed as different arguments
#'   (same convention as eval_preds())
compare_models <- function(...) {
  #browser()
  
  preds        <- list(...)
  names(preds) <- stringr::str_split_i(
    as.character(as.list(substitute(list(...)))[-1]),
    "_", 2
  )
  for (i in seq_along(preds)) {
    preds[[i]] <- preds[[i]] %>%
      mutate(model = names(preds)[i])
  }
  long <- data.table::rbindlist(preds, use.names = TRUE, fill = TRUE) %>%
    select(model, .rep, uniqueid, lppd, crps)
  
  model_names <- names(preds)
  pairs <- utils::combn(model_names, 2, simplify = FALSE)
  
  purrr::map_dfr(pairs, function(p) {
    elpd1 <- paste0("lppd_",p[1])
    elpd2 <- paste0("lppd_",p[2])
    crps1 <- paste0("crps_",p[1])
    crps2 <- paste0("crps_",p[2])
    long %>%
      filter(model %in% p) %>%
      tidyr::pivot_wider(names_from = model, values_from = c(lppd, crps)) %>%
      mutate(
        diff_elpd = .data[[elpd1]] - .data[[elpd2]],
        diff_crps = .data[[crps1]] - .data[[crps2]]
      ) %>%
      summarise(
        .by       = .rep,
        model_a   = p[1],
        model_b   = p[2],
        n         = n(),
        elpd_diff    = sum(diff_elpd),
        se_elpd_diff = sqrt(n * var(diff_elpd)),
        crps_diff    = mean(diff_crps),
        se_crps_diff = sd(diff_crps)/sqrt(n)
      )
  })
}
