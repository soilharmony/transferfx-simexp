# functions for MCMC diagnostics


#' @title mcmc_dx
#' @description 
#' Diagnostics of the MCMC sampling
#' @param df data.frame with posterior samples
mcmc_dx <- function(df) {
  # get list of parameter names for which to calculate MCMC-dx
  cnames <- colnames(df)
  params <- cnames[!grepl("\\[|\\.|__", cnames)]
  
  # r-hat
  rh <- function(dat) {
    sapply(params, function(x) rhat(extract_variable_matrix(dat, x)))
  }
  
  df.list <- split(df, df$.rep)
  as.data.frame(t(sapply(df.list, rh))) %>%
    rownames_to_column(var = ".rep")
}


#' @title combine_mcmcdx
#' @param ... list of models with MCMC samples
combine_mcmcdx <- function(...) {
  models <- list(...)
  names(models) <- stringr::str_split_i(
    as.character(as.list(substitute(list(...)))[-1]),
    "_", 2
  )
  for (i in 1:length(models)) {
    models[[i]] <- models[[i]] %>%
      mutate(model = names(models)[i])
  }
  data.table::rbindlist(models, use.names = TRUE, fill = TRUE)
}
