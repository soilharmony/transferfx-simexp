# predictions-simplex.R

#' @title compute_crps
#' @param data list of per-dataset Stan data lists (as passed into summarise_predictions())
#' @param draws_new_obs_long posterior predictive draws for new observations
compute_crps <- function(data, draws_new_obs_long) {
  
  # extract the observed reference method values (ILR-space)
  ilr_y_obs <- lapply(data, function(x) 
    tibble(.dataset_id   = x$.dataset_id,
           id            = as.character(x$psd_new$id),
           ilr_y_obs_new = asplit(as.matrix(x$ilr_y_obs_new), 1, drop = TRUE))
  ) %>% data.table::rbindlist()
  
  # bind with predictions of the reference method (ILR-space)
  draws_new_obs_long %>%
    mutate(
      # take a subsample of 1000 posterior predictive draws 
      # (from the original 4000) to speed up computation
      # this changes little to the results
      ilr_pred = map(ilr_pred, function(x) t(x %>% slice_sample(n = 1000)))
    ) %>% 
    left_join(ilr_y_obs, by = c(".dataset_id", "id")) %>%
    mutate(
      # calculate energy score (not vectorized)
      ES = map2_dbl(
        ilr_y_obs_new, ilr_pred, 
        function(yobs, ypred) {
          scoringRules::es_sample(yobs, ypred)
        }
      )
    ) %>%
    select(.dataset_id, .rep, id, ES)
}

