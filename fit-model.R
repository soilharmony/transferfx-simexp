# fitting candidate models
# detour by first fitting in STAN and then injecting it in brms


# -------------------------------------------------------------------------
# Bayesian linear regression (asymmetric model) ---------------------------

fit_linreg <- function(data) {
  fit_skeleton <- brm(
    y_obs ~ x_obs,
    data = data, empty = TRUE
  )
  stanfit <- stan(
    file = "stan/linreg.stan",
    data = list(
      N = nrow(data),
      y_obs = data$y_obs,
      x_obs = data$x_obs
    )
  )
  fit_skeleton$fit <- stanfit
  fit_custom <- rename_pars(fit_skeleton)
  return(fit_custom)
}


# -------------------------------------------------------------------------
# Bayesian error-in-variables regression (symmetric model) ----------------
# known sdmex

fit_eiv1 <- function(data, sigma_mex) {
  fit_skeleton <- brm(
    y_obs ~ x_obs,
    data = data, empty = TRUE
  )
  stanfit <- stan(
    file = "stan/eivreg_known_sdmex.stan",
    data = list(
      N = nrow(data),
      y_obs = data$y_obs,
      x_obs = data$x_obs,
      sigma_mex = sigma_mex
    )
  )
  fit_skeleton$fit <- stanfit
  fit_custom <- rename_pars(fit_skeleton)
  return(fit_custom)
}


# -------------------------------------------------------------------------
# Bayesian error-in-variables regression (symmetric model) ----------------
# unknown sdmex

fit_eiv2 <- function(data) {
  fit_skeleton <- brm(
    y_obs ~ x_obs,
    data = data, empty = TRUE
  )
  stanfit <- stan(
    file = "stan/eivreg_unknown_sdmex.stan",
    data = list(
      N = nrow(data),
      y_obs = data$y_obs,
      x_obs = data$x_obs
    )
  )
  fit_skeleton$fit <- stanfit
  fit_custom <- rename_pars(fit_skeleton)
  return(fit_custom)
}

