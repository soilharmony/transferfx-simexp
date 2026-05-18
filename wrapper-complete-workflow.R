# wrapper around complete workflow

#' single_rep
single_rep <- function(data_train, data_test, model, sigma_mex) {
  
  # (1) fit model
  model <- switch(
    model,
    linreg  = fit_linreg(data_train),
    eivreg1 = fit_eiv1(data_train, sigma_mex),
    eivreg2 = fit_eiv2(data_train)
  )
  
  # (2) predict new data
  preds <- predict_new(model, data_test, .95)
  
  # (3) evaluate predictions
  metrics <- pred_eval(preds, data_test)
  
  return(metrics)
}
