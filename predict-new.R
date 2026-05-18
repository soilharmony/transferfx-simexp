# predict-new.R


#' predict_new
#'
#' @description
#' Function to use a fitted model to predict the value of new observations with a prediction interval.
#'
#' @param brmsfit An object of class `brmsfit`.
#' @param newdata A data.frame containing the predictors of the new data for which to make predictions.
#' @param level Level of uncertainty for the prediction interval (default .95).
#'
#' @export
predict_new <- function(brmsfit, newdata, level = .95) {
  stopifnot("brmsfit" %in% class(brmsfit))
  yhat    <- fitted(brmsfit, newdata, summary = F)[, 1]
  yhatCrI <- predictive_interval(brmsfit, newdata, prob = level)
  colnames(yhatCrI) <- c("lwr","upr")
  return(data.frame(yhat = yhat, yhatCrI))
}



#' pred_eval
#'
#' @description
#' This function compares the predictions of a model against the known (simulated) values and computes several metrics to gauge the performance of the model.
#'
#' @param preds R data.frame with the predicted values
#' @param true R data.frame with the true values
#'
#' @export
pred_eval <- function(preds, true) {
  # combine datasets for evaluation
  dat <- cbind(preds, true)

  # coverage (PICP = prediction interval coverage probability)
  PICP <- dat %>%
    summarise(PICP = mean(between(y_true, lwr, upr))) %>%
    pull(PICP)

  # root mean-square error
  RMSE <- dat %>%
    summarise(RMSE = sqrt(mean((yhat - y_true)^2))) %>%
    pull(RMSE)

  # mean absolute error
  MAE <- dat %>%
    summarise(MAE = mean(abs(yhat - y_true))) %>%
    pull(MAE)

  # mean prediction error
  MPE <- dat %>%
    summarise(MPE = mean(yhat - y_true)) %>%
    pull(MPE)

  # stand.dev of prediction errors
  SDPE <- dat %>%
    mutate(pred_err = yhat - y_true) %>%
    summarise(SDPE = sd(pred_err)) %>%
    pull(SDPE)

  # R2
  R2 <- dat %>%
    summarise(
      R2 = 1 - sum((y_true - yhat)^2) / sum((y_true - mean(y_true))^2)
    ) %>%
    pull(R2)

  # all metrics together
  metrics <- data.frame(
    PICP = PICP,
    RMSE = RMSE,
    MAE = MAE,
    MPE = MPE,
    SDPE = SDPE,
    R2 = R2
  )
  return(metrics)
}
