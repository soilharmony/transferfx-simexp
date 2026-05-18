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
                         .options = furrr_options(seed = T), .progress = T),
    eivreg1 = future_map2(data_train, sigma_mex, fit_eiv1, 
                          .options = furrr_options(seed = T), .progress = T),
    eivreg2 = future_map(data_train, fit_eiv2, 
                         .options = furrr_options(seed = T), .progress = T)
  )

# next the predictions on a new dataset
simexp_results2 <- simexp_results %>%
  mutate(
    linreg_pred  = future_map2(linreg, data_test, predict_new,
                               .options = furrr_options(seed = T), 
                               .progress = T),
    eivreg1_pred = future_map2(eivreg1, data_test, predict_new,
                               .options = furrr_options(seed = T), 
                               .progress = T),
    eivreg2_pred = future_map2(eivreg2, data_test, predict_new,
                               .options = furrr_options(seed = T), 
                               .progress = T)
  )

# next the evaluation of the predictions
simexp_results3 <- simexp_results2 %>%
  mutate(
    lin_reg_metrics = map2(lin_reg_pred, data_test, pred_eval),
    eiv_reg_metrics = map2(eiv_reg_pred, data_test, pred_eval)
  )
View(simexp_results3)

plot(
  simexp_results2$lin_reg_pred[[2]]$yhat,
  simexp_results2$data_test[[2]]$y_true)

# basic figure with results





