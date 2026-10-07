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


#' @title mcmc_dx_simplex
#' @description 
#' Diagnostics of the MCMC sampling
#' @param df data.frame with posterior samples
mcmc_dx_simplex <- function(df) {
  params <- c("beta",
              "L_Omega", "l_sigma",                   # mvlinreg
              "L_Omega_y", "l_sigma_y",               # mvlinreg_knownSDme
              "mu_x", "L_Omega_x", "l_sigma_x",
              "L_Omega_mex", "l_sigma_mex")           # mvlinreg_unknownSDme
  
  # scalar columns belonging to a directly sampled parameter:
  # strip the index part ("beta[1,2]" -> "beta") and match exactly
  cnames <- colnames(df)
  base   <- sub("\\[.*$", "", cnames)
  vars   <- cnames[base %in% params]
  
  # drop constants (fixed elements of Cholesky factors -> R-hat undefined)
  is_const <- vapply(df[vars], function(x) isTRUE(all.equal(var(x), 0)), 
                     logical(1))
  vars <- vars[!is_const]
  
  # r-hat for all selected variables within one replicate
  rh <- function(dat) {
    draws <- as_draws_df(dat[, c(".chain", ".iteration", ".draw", vars)])
    data.frame(
      variable = vars,
      rhat     = vapply(vars, function(v) {
        rhat(posterior::extract_variable_matrix(draws, v))
      }, numeric(1)),
      row.names = NULL
    )
  }
  
  df.list <- split(df, df$.rep)
  res <- do.call(rbind, lapply(names(df.list), function(r) {
    out <- rh(df.list[[r]])
    out$.rep <- r
    out
  }))
  res$param <- sub("\\[.*$", "", res$variable)
  
  # parameters grouped together, in the order given in `params`
  res$param <- factor(res$param, levels = intersect(params, unique(res$param)))
  res <- res[order(res$.rep, res$param), c(".rep", "param", "variable", "rhat")]
  rownames(res) <- NULL
  return(res)
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
