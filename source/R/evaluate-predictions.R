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
      MAE  = mean(abs(yhat - y_true)),
      RMSE = sqrt(mean((yhat - y_true)^2)),
      MPE  = mean(yhat - y_true),
      R2   = 1 - sum((y_true - yhat)^2) / sum((y_true - mean(y_true))^2),
      SDPE = sd(yhat - y_true),
      PICP = mean(between(y_true, yhat_ll, yhat_ul))
    ) %>%
    mutate(model = modelname, .before = .rep)
}
