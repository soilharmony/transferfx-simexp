# simulation-experiment.R

library(tidyverse)
library(rstan)
library(brms)
library(mirai)

# dataset
load(here::here("simulation-data/simexp-data.rda"))

# functions to fit models
source("fit-model.R")
source("predict-new.R")
source("wrapper-complete-workflow.R")

# complete workflow
daemons(6)
simexp_results <- simexp_data %>%
  mutate(
    sdmex = 1 * ratio_sdmex_sigmax,
    model_metrics = pmap(
      .l = list(data_train = data_train,
                data_test  = data_test,
                model      = models,
                sdmex      = sdmex),
      .f = in_parallel(
        \(data_train, data_test, model, sdmex) 
        single_rep(data_train, data_test, model, sdmex),
        # explicitly pass everything the workers need
        single_rep     = single_rep,
        fit_linreg     = fit_linreg,
        fit_eiv1       = fit_eiv1,
        fit_eiv2       = fit_eiv2,
        predict_new    = predict_new,
        pred_eval      = pred_eval,
        wrap_stan_brms = wrap_stan_brms,
        brm_skeleton   = brm_skeleton,
        sampler        = sampler,
        linreg_empty   = linreg_empty,
        eivreg1_empty  = eivreg1_empty,
        eivreg2_empty  = eivreg2_empty
      )
    )
  )
daemons(0)
save(simexp_results,
     file = here::here("simulation-data/simexp-results.rda"))
load(here::here("simulation-data/simexp-results.rda"))


# basic figure with results
simexp_results %>%
  select(sample_size, ratio_sdmex_sigmax, tails, models, model_metrics) %>%
  unnest(model_metrics) %>%
  ggplot(aes(y = value, 
             x = factor(ratio_sdmex_sigmax), 
             shape = factor(sample_size), 
             color = models)) +
  facet_wrap(~ metric, scales = "free_y") +
  geom_jitter(height = 0, width = .1) +
  labs(y = "", x = expression(sigma[u] / sigma[x]),
       shape = "sample size", color = "models")
  

# focus on a single metric
simexp_results %>%
  select(sample_size, ratio_sdmex_sigmax, tails, models, model_metrics) %>%
  unnest(model_metrics) %>%
  filter(metric == "PICP") %>%
  ggplot(aes(factor(ratio_sdmex_sigmax), value, color = models)) +
  facet_grid(tails ~ sample_size) +
  geom_jitter(height = 0, width = .1)


