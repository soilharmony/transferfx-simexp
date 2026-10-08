# evaluate-predictions-simplex.R
#
# set of R functions to evaluate the predictions in experiments with 
# simplex-distributed variables


#' summarise_predictions
#' summarise predictions and bind with validation data
#' @param mcmc posterior draws
#' @param data list of per-dataset Stan data lists
summarise_predictions <- function(mcmc, data) {
  
  draws_new_obs <- mcmc %>%
    select(.dataset_id, .rep, contains("ilr_y_obs_new_rep"))
  
  spc <- data.frame(
    .name = colnames(draws_new_obs)[3:ncol(draws_new_obs)]
  ) %>%
    mutate(
      .value = case_when(grepl(",1]", .name) ~ "X1",
                         grepl(",2]", .name) ~ "X2"),
      id = stringr::str_extract(.name, "(?<=\\[)\\d+")
    )
  
  draws_new_obs_long <- draws_new_obs %>%
    pivot_longer_spec(spc) %>%
    nest(.by = c(.dataset_id, .rep, id), .key = "ilr_pred")
  
  validation_data <- data.table::rbindlist(
    lapply(data, get_validation_data)
  )

  # calculate prediction errors in the ILR-space
  pred_summary <- validation_data %>%
    left_join(draws_new_obs_long, by = c(".dataset_id","id")) %>%
    mutate(
      err = map2(ilr_obs, ilr_pred, .f = ilr_err)
    ) %>%
    unnest(err) %>%
    select(.dataset_id, .rep, id, ilr_obs_mean_dist, adist, in_ellipse)
  
  # calculate multivariate CRPS (energy score)
  crps <- compute_crps(data, draws_new_obs_long)
  
  left_join(pred_summary, crps, by = c(".dataset_id", ".rep", "id"))
}


#' get_validation_data
#' helper function for summarise_predictions()
get_validation_data <- function(data) {
  
  tmp <- data.frame(
    .dataset_id = rep(data$.dataset_id, data$N_new),
    id          = as.character(data$psd_new$id),
    y_obs_clay  = data$psd_new$y_obs_clay,
    y_obs_silt  = data$psd_new$y_obs_silt,
    y_obs_sand  = data$psd_new$y_obs_sand
  ) %>%
    nest(.by = c(.dataset_id, id), .key = "psd_obs") %>%
    mutate(ilr_obs = map(psd_obs, compositions::ilr))
  
  # for use in R² calculations: distance between the ILR-coordinate and the mean
  # of all observations
  tmp <- tmp %>%
    mutate(
      ilr_obs_mean = list(colMeans(reduce(tmp$ilr_obs, rbind))),
      ilr_obs_mean_dist = map2_dbl(
        ilr_obs, ilr_obs_mean, 
        function(obs, mean) {
          as.numeric(stats::dist(as.numeric(obs - mean), "euclidean")) 
        }
      )
    )
  
  return(tmp)
}

#' ilr_err
#' helper function for summarise_predictions()
ilr_err <- function(obs, pred) {
  
  # Aitchinson distance between obs & point estimate (bias):
  est   <- colMeans(pred) # point-estimate in ILR-space
  adist <- as.numeric(stats::dist(as.numeric(obs - est), "euclidean"))
  
  # 95% prediction ellipse coverage:
  d2          <- mahalanobis(obs, colMeans(pred), cov(pred))
  in_ellipse  <- d2 <= 5.991465 # qchisq(.95, df = 2) = 5.991465
  
  return(data.frame(adist = adist, in_ellipse = in_ellipse))
}


#' eval_preds_simplex
#' evaluate prediction models for simplex-distributed variables
#' @param ... set of models to evaluate, passed as different arguments
eval_preds_simplex <- function(...) {
  preds  <- list(...)
  names(preds) <- stringr::str_split_i(
    as.character(as.list(substitute(list(...)))[-1]),
    "_", 2
  )
  for (i in 1:length(preds)) {
    preds[[i]] <- preds[[i]] %>%
      mutate(model = names(preds)[i])
  }
  data.table::rbindlist(
    lapply(preds, val_metrics_simplex)
  )
}

#' val_metrics_simplex
#' helper function for `eval_preds_simplex()`: 
#' calculation of the validation metrics
val_metrics_simplex <- function(df) {
  df %>%
    summarise(
      .by = c(model, .rep),
      MAD   = mean(adist),       # mean Aitchinson distance
      PECP  = mean(in_ellipse),  # prediction ellipse coverage probability
      R2    = 1 - (sum(adist^2) / sum(ilr_obs_mean_dist^2)), # R²
      ES    = mean(ES),          # mean energy score
      ES_SE = sd(ES) / sqrt(n()) # se of the energy score
    ) 
}

#' @title compare_models
#' @description
#' Paired CRPS model comparison. 
#' @param ... set of models to compare, passed as different arguments
compare_models <- function(...) {
  #browser()
  
  preds        <- list(...)
  names(preds) <- stringr::str_split_i(
    as.character(as.list(substitute(list(...)))[-1]),
    "_", 2
  )
  for (i in seq_along(preds)) {
    preds[[i]] <- preds[[i]] %>%
      mutate(model = names(preds)[i])
  }
  long <- data.table::rbindlist(preds, use.names = TRUE, fill = TRUE) %>%
    select(model, .rep, id, ES)
  
  model_names <- names(preds)
  pairs <- utils::combn(model_names, 2, simplify = FALSE)
  
  purrr::map_dfr(pairs, function(p) {
    long %>%
      filter(model %in% p) %>%
      tidyr::pivot_wider(names_from = model, values_from = ES) %>%
      mutate(
        diff_crps = .data[[p[1]]] - .data[[p[2]]]
      ) %>%
      summarise(
        .by          = .rep,
        model_a      = p[1],
        model_b      = p[2],
        n            = n(),
        crps_diff    = mean(diff_crps),
        se_crps_diff = sd(diff_crps)/sqrt(n)
      )
  })
}
