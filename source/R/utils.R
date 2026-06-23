# utility functions


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
#' @param linreg ....
#' @param eivreg1 ....
#' @param eivreg2 ....
combine_mcmcdx <- function(linreg, eivreg1, eivreg2) {
  data.table::rbindlist(
    list(
      linreg %>% mutate(model = "linreg"),
      eivreg1 %>% mutate(model = "eivreg1"),
      eivreg2 %>% mutate(model = "eivreg2")
    ),
    use.names = TRUE, fill = TRUE
  )
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


#' @title tidy_scenario
#' @description
#' Extract scenario parameters from the scenario column
#' @param df ....
tidy_scenario <- function(df) {

  df %>%
    separate_wider_delim(
      scenario, "_", 
      names = c("x","sample_size","taux","tauxy","corrme",
                "tails","mux","sigmax","alpha","beta","sigmay_struct",
                "ratio_sdme")
    ) %>%
    mutate(
      alpha = case_when(alpha == "Alpha.0.5" ~ "Alpha-0.5", TRUE ~ alpha),
      across(c(sample_size, taux, tauxy, corrme, 
               mux, sigmax, alpha, beta, sigmay_struct, ratio_sdme), 
             ~ as.numeric(gsub("[^0-9.-]", "", .x))),
      tails = substr(tails, 5, nchar(tails))
    ) %>%
    select(-x)
}

