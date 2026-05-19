# simulation-experiment.R

library(tidyverse)
library(rstan)
library(brms)
library(furrr)

# dataset
load(here::here("simulation-data/simexp-data.rda"))

# functions to fit models
source("fit-model.R")
source("predict-new.R")
source("wrapper-complete-workflow.R")

# set up parallel workers
plan(multisession, workers = 6)

# complete workflow
simexp_results <- simexp_data %>%
  mutate(
    sdmex = 1 * ratio_sdmex_sigmax,
    model_metrics = future_pmap(
      .f = single_rep,
      .l = list(data_train = data_train,
                data_test  = data_test, 
                model      = models,
                sdmex      = sdmex),
      .options = furrr_options(seed = T),
      .progress = T
    )
  )
save(simexp_results,
     file = here::here("simulation-data/simexp_results.rda"))
load(here::here("simulation-data/simexp_results.rda"))


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


