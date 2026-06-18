# utility functions

#' @title tidy_predeval_summary
#' @description
#' Extract scenario parameters from the scenario column
#' @param df ....
tidy_predeval_summary <- function(df) {
  df %>%
    separate_wider_delim(
      scenario, "_", names = c("x","sample_size","tau","tails")
    ) %>%
    mutate(
      sample_size = as.numeric(gsub("[^0-9.]", "", sample_size)),
      tau = as.numeric(gsub("[^0-9.]", "", tau)),
      tails = substr(tails, 5, nchar(tails))
    ) %>%
    select(sample_size, tau, tails,
           model, .rep, 
           MAE, RMSE, MPE, R2, SDPE, PICP)
}


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

#' @title tidy_mcmcdx
#' @description
#' Extract scenario parameters from the scenario column
#' @param df ....
tidy_mcmcdx <- function(df) {
  df %>%
    separate_wider_delim(
      scenario, "_", names = c("x","model","sample_size","tau","tails")
    ) %>%
    mutate(
      sample_size = as.numeric(gsub("[^0-9.]", "", sample_size)),
      tau = as.numeric(gsub("[^0-9.]", "", tau)),
      tails = substr(tails, 5, nchar(tails))
    ) %>%
    select(-c(x, model))
}


#' @title reg_dilution
#' @description
#' Quantify how much regression dilution was created by the measurement error. True beta used in the simulation was 1.
#' @param data Training dataset for the MCMC.
reg_dilution <- function(data) {
  beta_obs <- sapply(
    data, function(x) unname(coef(lm(y_obs ~ x_obs, data = x))[2])
  )
  tibble(beta_obs = beta_obs)
}


#' @title tidy_regdilution
#' @description
#' Extract scenario parameters from the scenario column
#' @param df ....
tidy_regdilution <- function(df) {
  df %>%
    separate_wider_delim(
      scenario, "_", names = c("x","sample_size","tau","tails")
    ) %>%
    mutate(
      sample_size = as.numeric(gsub("[^0-9.]", "", sample_size)),
      tau = as.numeric(gsub("[^0-9.]", "", tau)),
      tails = substr(tails, 5, nchar(tails))
    ) %>%
    select(sample_size, tau, tails, beta_obs)
}

