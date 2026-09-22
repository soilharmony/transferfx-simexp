# predictions-simplex.R
# Exact pointwise log predictive density (lppd) for the simplex-experiment
# Stan models. This is deliberately kept separate from the Monte-Carlo
# based yhat/interval logic in evaluate-predictions-simplex.R: yhat/yhat_ll/
# yhat_ul are already fine using the `y_new_rep` posterior predictive
# draws Stan already generates, but lppd needs the *analytic* density, so it
# can't be read off `y_new_rep` alone.

#' @title compute_lppd_crps
#' @description
#' Dispatches to the correct density family and returns a data.frame of
#' `.dataset_id`, `uniqueid`, `lppd`, `crps` -- ready to left_join() onto the
#' `preds` data.frame built from `y_new_rep` in `summarise_predictions()`.
#' @param mcmc posterior draws (as passed into summarise_predictions())
#' @param data list of per-dataset Stan data lists (as passed into
#'   summarise_predictions())
compute_lppd_crps <- function(mcmc, data) {
  
  # index the data list by .dataset_id for lookup inside the map below
  data_by_id <- setNames(data, sapply(data, function(x) x$.dataset_id))
  
  draws_nested <- mcmc %>%
    select(.dataset_id, shape, contains("new")) %>%
    nest(.by = .dataset_id, .key = "draws")

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


#' helper function for compute_lppd_crps
#' @param draws data.frame of posterior draws with columns `L_Sigma` and
#'   `mu_new[1]`...`mu_new[N_new]`
#' @param d the Stan data list for this dataset (needs `N_new`, `y_obs_new`)
lppd_fn <- function(draws, d) {
  
}
