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
      PICP = mean(between(y_obs, yhat_ll, yhat_ul))
    ) 
}
