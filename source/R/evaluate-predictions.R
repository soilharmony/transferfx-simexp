# evaluate predictions against validation data

#' @title eval_preds
#' @param linreg ....
#' @param eivreg1 ....
#' @param eivreg2 ....
eval_preds <- function(linreg, eivreg1, eivreg2) {
  rbind(
    val_metrics(linreg,  "linreg"),
    val_metrics(eivreg1, "eivreg1"),
    val_metrics(eivreg2, "eivreg2")
  )
}

#' @title val_metrics
#' @description
#' Calculation of the validation metrics
#' 
#' @param df ....
#' @param modelname ....
val_metrics <- function(df, modelname) {
  df %>%
    summarise(
      .by = .rep,
      MAE  = mean(abs(yhat - y_obs)),
      RMSE = sqrt(mean((yhat - y_obs)^2)),
      MPE  = mean(yhat - y_obs),
      R2   = 1 - sum((y_obs - yhat)^2) / sum((y_obs - mean(y_obs))^2),
      SDPE = sd(yhat - y_obs),
      PICP = mean(between(y_obs, yhat_ll, yhat_ul))
    ) %>%
    mutate(model = modelname, .before = .rep)
}
