# wrapper around complete workflow

#' single_rep
#' 
#' @description
#' A short description...
#' 
#' @param data_train Training dataset
#' @param data_test Independent test dataset for validation
#' @param model Name of the model ("linreg", "eivreg1", "eivreg2")
#' @param sdmex Known value of the SD of the measurement error in x (for use in "eivreg1")
#' 
single_rep <- function(data_train, data_test, model, sdmex) {
  
  # (1) fit model
  model <- switch(
    model,
    linreg  = fit_linreg(data_train),
    eivreg1 = fit_eiv1(data_train, sdmex),
    eivreg2 = fit_eiv2(data_train)
  )
  
  # (2) predict new data
  preds <- predict_new(model, data_test, .95)
  
  # (3) evaluate predictions
  metrics <- pred_eval(preds, data_test)
  
  return(metrics)
}
