# simulation-experiment.R

library(tidyverse)
library(rstan)
library(brms)
library(furrr)

# dataset
load("simulation-data/simexp-data.rda")

# functions to fit models
source("fit-model.R")
source("predict-new.R")


# set up parallel workers
plan(multisession, workers = 4)

# fit candidate models on training data
simexp_results <- simexp_data %>%
  mutate(
    sigma_mex = 1 * ratio_varmex_varx,
    linreg  = future_map(data_train, fit_linreg, 
                         .options = furrr_options(seed = T), 
                         .progress = T),
    eivreg1 = future_map2(data_train, sigma_mex, fit_eiv1, 
                          .options = furrr_options(seed = T), 
                          .progress = T),
    eivreg2 = future_map(data_train, fit_eiv2, 
                         .options = furrr_options(seed = T), 
                         .progress = T)
  )

# next the predictions on a new dataset
simexp_results2 <- simexp_results %>%
  mutate(
    linreg_pred  = map2(linreg, data_test, predict_new,
                        .progress = T),
    eivreg1_pred = map2(eivreg1, data_test, predict_new,
                        .progress = T),
    eivreg2_pred = map2(eivreg2, data_test, predict_new,
                        .progress = T)
  )

# next the evaluation of the predictions
simexp_results3 <- simexp_results2 %>%
  mutate(
    linreg_metrics  = map2(linreg_pred, data_test, pred_eval),
    eivreg1_metrics = map2(eivreg1_pred, data_test, pred_eval),
    eivreg2_metrics = map2(eivreg2_pred, data_test, pred_eval)
  )

# basic figure with results
simexp_results3 %>%
  select(sample_size, ratio_varmex_varx, tails, contains("metrics")) %>%
  pivot_longer(contains("metrics"), 
               names_to = "models", values_to = "metrics") %>%
  mutate(models = stringr::str_split_i(models, "_", 1)) %>%
  unnest(cols = contains("metrics"))




