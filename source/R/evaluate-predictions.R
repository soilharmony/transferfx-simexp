# evaluate predictions against validation data

#' @title eval_preds
#' @param ... set of models to evaluate, passed as different arguments
eval_preds <- function(...) {
  preds        <- list(...)
  names(preds) <- stringr::str_split_i(
    as.character(as.list(substitute(list(...)))[-1]),
    "_", 2
  )
  for (i in 1:length(preds)) {
    preds[[i]] <- preds[[i]] %>%
      mutate(model = names(preds)[i])
  }
  data.table::rbindlist(
    lapply(preds, val_metrics)
  )
}

#' @title val_metrics
#' @description
#' Calculation of the validation metrics
#' 
#' @param df data.frame with predictions
val_metrics <- function(df) {
  df %>%
    summarise(
      .by = c(model, .rep),
      MAE  = mean(abs(yhat - y_obs)),
      RMSE = sqrt(mean((yhat - y_obs)^2)),
      MPE  = mean(yhat - y_obs),
      R2   = 1 - sum((y_obs - yhat)^2) / sum((y_obs - mean(y_obs))^2),
      SDPE = sd(yhat - y_obs),
      PICP = mean(between(y_obs, yhat_ll, yhat_ul)),
      ELPD    = sum(lppd), # Exact Expected Log Predictive Density
      ELPD_SE = sqrt(n() * var(lppd)),
      CRPS    = mean(crps),
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


