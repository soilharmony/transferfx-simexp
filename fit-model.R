# fitting candidate models


# sampler function with common MCMC settings
sampler <- function(model, data) {
  ### note: iter/wu == 1000/500 for testing, need more for real experiment
  rstan::sampling(model, data, chains = 4, iter = 1000, warmup = 500,
                  thin = 1, seed = 123, cores = 4, refresh = 0)
}


# detour by first fitting in STAN and then injection in brms 
# (for easier post-processing)
brm_skeleton <- brms::brm(
  y_obs ~ 1 + x_obs,
  data = data.frame(x_obs = 0, y_obs = 0), 
  empty = TRUE
)
wrap_stan_brms <- function(data, model, skeleton = brm_skeleton) {
  skeleton$fit <- sampler(model, data)
  brms::rename_pars(skeleton)
}


# -------------------------------------------------------------------------
# Bayesian linear regression (asymmetric model) ---------------------------

### pre-compile model, save and load (only once)
# linreg_empty <- stan_model(
#   file = here::here("stan/linreg.stan")
# )
# write_rds(linreg_empty, here::here("stan/linreg.rds"))
linreg_empty <- read_rds(here::here("stan/linreg.rds"))

fit_linreg <- function(data) {
  data = list( 
    N = nrow(data), 
    y_obs = data$y_obs,
    x_obs = data$x_obs
  )
  fit <- wrap_stan_brms(data, linreg_empty)
  return(fit)
}


# -------------------------------------------------------------------------
# Bayesian error-in-variables regression (symmetric model) ----------------
# known sdmex

### pre-compile model, save and load (only once)
# eivreg1_empty <- stan_model(
#   file = here::here("stan/eivreg_known_sdmex.stan")
# )
# write_rds(eivreg1_empty, here::here("stan/eivreg_known_sdmex.rds"))
eivreg1_empty <- read_rds(here::here("stan/eivreg_known_sdmex.rds"))

fit_eiv1 <- function(data, sigma_mex) {
  data = list( 
    N = nrow(data), 
    y_obs = data$y_obs,
    x_obs = data$x_obs,
    sigma_mex = sigma_mex
  )
  fit <- wrap_stan_brms(data, eivreg1_empty)
  return(fit)
}


# -------------------------------------------------------------------------
# Bayesian error-in-variables regression (symmetric model) ----------------
# unknown sdmex

### pre-compile model, save and load (only once)
# eivreg2_empty <- stan_model(
#   file = here::here("stan/eivreg_unknown_sdmex.stan")
# )
# write_rds(eivreg2_empty, here::here("stan/eivreg_unknown_sdmex.rds"))
eivreg2_empty <- read_rds(here::here("stan/eivreg_unknown_sdmex.rds"))

fit_eiv2 <- function(data) {
  data = list( 
    N = nrow(data), 
    y_obs = data$y_obs,
    x_obs = data$x_obs
  )
  fit <- wrap_stan_brms(data, eivreg2_empty)
  return(fit)
}

