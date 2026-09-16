# predictions-gamma.R
# Exact pointwise log predictive density (lppd) for the gamma-experiment
# Stan models. This is deliberately kept separate from the Monte-Carlo
# based yhat/interval logic in evaluate-predictions-gamma.R: yhat/yhat_ll/
# yhat_ul are already fine using the `y_new_rep` posterior predictive
# draws Stan already generates, but lppd needs the *analytic* density,
# which differs by likelihood family (Gamma vs Normal vs LogNormal), so it
# can't be read off `y_new_rep` alone.
#
# All three model families in this experiment reduce to the same underlying
# math already used in predictions-normal.R: build an
# N x D matrix of the relevant density evaluated at the true held-out
# y_obs_new, then matrixStats::rowLogSumExps() - log(D) per observation.


#' @title compute_lppd_crps
#' @description
#' Dispatches to the correct density family and returns a data.frame of
#' `.dataset_id`, `uniqueid`, `lppd`, `crps` -- ready to left_join() onto the
#' `preds` data.frame built from `y_new_rep` in `summarise_predictions()`.
#' @param mcmc posterior draws (as passed into summarise_predictions())
#' @param data list of per-dataset Stan data lists (as passed into
#'   summarise_predictions())
#' @param family one of "gamma", "normal", "lognormal"
compute_lppd_crps <- function(
    mcmc, data, family = c("gamma", "normal", "lognormal")
) {
  family <- match.arg(family)

  # index the data list by .dataset_id for lookup inside the map below
  data_by_id <- setNames(data, sapply(data, function(x) x$.dataset_id))

  lppd_fn <- switch(
    family,
    gamma     = lppd_crps_gamma_matrix,
    normal    = lppd_crps_normal_matrix,
    lognormal = lppd_crps_lognormal_matrix
  )

  draws_nested <- if (family == "gamma") {
    mcmc %>%
      select(.dataset_id, shape, contains("new")) %>%
      nest(.by = .dataset_id, .key = "draws")
  } else {
    mcmc %>%
      select(.dataset_id, beta0, beta1, sigma, contains("y_new_rep")) %>%
      nest(.by = .dataset_id, .key = "draws")
  }

  draws_nested %>%
    mutate(
      lppd_crps_df = purrr::map2(
        draws, .dataset_id,
        function(draws, id) lppd_fn(draws, data_by_id[[as.character(id)]])
      )
    ) %>%
    select(.dataset_id, lppd_crps_df) %>%
    unnest(lppd_crps_df)
}

#' @title lppd_crps_gamma_matrix
#' @description
#' Exact pointwise lppd for the Gamma-likelihood models (gammareg.stan and
#' all gammareg_eiv_*.stan variants) -- these all expose `mu_new[1..N_new]`
#' and `shape` directly as posterior draws, so one function covers all of
#' them.
#' @param draws data.frame of posterior draws with columns `shape` and
#'   `mu_new[1]`...`mu_new[N_new]`
#' @param d the Stan data list for this dataset (needs `N_new`, `y_obs_new`)
lppd_crps_gamma_matrix <- function(draws, d) {
  
  N <- d$N_new
  D <- nrow(draws)

  mu_mat <- t(as.matrix(select(draws, contains("mu_new"))))  # N x D
  shape  <- draws$shape
  ydraw_mat <- draws %>%
    select(contains("y_new_rep")) %>%
    as.matrix()
  
  log_lik_mat <- matrix(NA_real_, nrow = N, ncol = D)
  for (dd in 1:D) {
    rate_dd <- shape[dd] / mu_mat[, dd]
    log_lik_mat[, dd] <- dgamma(
      d$y_obs_new, shape = shape[dd], rate = rate_dd, log = TRUE
    )
  }
  lppd_pointwise <- matrixStats::rowLogSumExps(log_lik_mat) - log(D)
  crps <- -1 * scoringRules::crps_sample(
    dat = t(ydraw_mat),
    y   = d$y_obs_new
  )
  data.frame(uniqueid = seq_len(N), lppd = lppd_pointwise, crps = crps)
}

#' @title lppd_crps_normal_matrix
#' @description
#' Exact pointwise lppd for gamma_linreg.stan (plain Gaussian likelihood on
#' the raw y/x scale). Unlike the Gamma models, this model doesn't expose a
#' precomputed linear predictor for the new data, so it's built here from
#' `beta0`/`beta1` and `x_obs_new`.
#' @param draws data.frame of posterior draws with columns `beta0`,
#'   `beta1`, `sigma`
#' @param d the Stan data list (needs `N_new`, `x_obs_new`, `y_obs_new`)
lppd_crps_normal_matrix <- function(draws, d) {
  
  N <- d$N_new
  D <- nrow(draws)

  X <- cbind(1, d$x_obs_new)
  B <- cbind(draws$beta0, draws$beta1)
  eta_mat <- X %*% t(B)  # N x D
  ydraw_mat <- draws %>%
    select(contains("y_new_rep")) %>%
    as.matrix()

  log_lik_mat <- matrix(NA_real_, nrow = N, ncol = D)
  for (dd in 1:D) {
    log_lik_mat[, dd] <- dnorm(
      d$y_obs_new, mean = eta_mat[, dd], sd = draws$sigma[dd], log = TRUE
    )
  }
  lppd_pointwise <- matrixStats::rowLogSumExps(log_lik_mat) - log(D)
  crps <- -1 * scoringRules::crps_sample(
    dat = t(ydraw_mat),
    y   = d$y_obs_new
  )
  # sanity check: plot(lppd_pointwise, crps) => strong positive correlation
  data.frame(uniqueid = seq_len(N), lppd = lppd_pointwise, crps = crps)
}

#' @title lppd_crps_lognormal_matrix
#' @description
#' Exact pointwise lppd for gamma_linreglogtrafo.stan, evaluated on the
#' ORIGINAL y scale via dlnorm() -- NOT dnorm(log(y), ...), which would
#' silently drop the Jacobian term log(1/y) and make this model's lppd
#' incomparable to the Gaussian/Gamma models' lppd (see earlier review:
#' beta0 + beta1*log(x) is the conditional log-median here, and dlnorm()
#' is what correctly turns that into a density on the original scale).
#' @param draws data.frame of posterior draws with columns `beta0`,
#'   `beta1`, `sigma` (all on the log scale)
#' @param d the Stan data list (needs `N_new`, `x_obs_new`, `y_obs_new`)
lppd_crps_lognormal_matrix <- function(draws, d) {
  
  N <- d$N_new
  D <- nrow(draws)

  X <- cbind(1, log(d$x_obs_new))
  B <- cbind(draws$beta0, draws$beta1)
  eta_log_mat <- X %*% t(B)  # N x D, log-scale linear predictor
  ydraw_mat <- draws %>%
    select(contains("y_new_rep")) %>%
    as.matrix()
  
  log_lik_mat <- matrix(NA_real_, nrow = N, ncol = D)
  for (dd in 1:D) {
    log_lik_mat[, dd] <- dlnorm(
      d$y_obs_new,
      meanlog = eta_log_mat[, dd], sdlog = draws$sigma[dd], log = TRUE
    )
  }
  lppd_pointwise <- matrixStats::rowLogSumExps(log_lik_mat) - log(D)
  crps <- -1 * scoringRules::crps_sample(
    dat = t(ydraw_mat),
    y   = d$y_obs_new
  )
  data.frame(uniqueid = seq_len(N), lppd = lppd_pointwise, crps = crps)
}
