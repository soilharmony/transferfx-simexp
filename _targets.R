# _targets.R
library(targets)
library(tarchetypes)
library(stantargets)
library(posterior) |> suppressPackageStartupMessages()
library(tidyverse) |> suppressPackageStartupMessages()
library(here) |> suppressPackageStartupMessages()
library(quarto)
library(crew)

tar_option_set(
  controller = crew_controller_local(workers = 2)
)
tar_source(
  files = here("source", "R")
)

### ---------------- ###
### Targets-pipeline ###
### ---------------- ###
list(
  # part of the pipeline to map over scenario's:
  mapped <- tar_map(
    unlist = FALSE,
    values = simexp_design,
    names  = all_of(c("sample_size_label", 
                      "ratio_sdmex_sigmax_label", 
                      "tails_label")),
    
    # for each batch/rep: draw simulated data and fit Bayesian models on it
    # the function sim_data returns both a training and a validation dataset
    tar_stan_mcmc_rep_draws(
      name       = mcmc,
      stan_files = c(here("source/stan/linreg.stan"),
                     here("source/stan/eivreg_known_sdmex.stan"),
                     here("source/stan/eivreg_unknown_sdmex.stan")),
      data = sim_data(
        N                  = sample_size,
        ratio_sdmex_sigmax = ratio_sdmex_sigmax,
        ratio_sdmey_sdmex  = 1,
        tails              = tails
      ),
      seed          = 123,
      chains        = 4, parallel_chains = 4,
      iter_warmup   = 1000,
      iter_sampling = 1000,
      refresh       = 0,
      batches       = 2,
      reps          = 5,
      stdout = R.utils::nullfile(),
      stderr = R.utils::nullfile()
    ),
    
    # quantification of regression dilution (in the training data)
    tar_target(
      regdilution,
      reg_dilution(mcmc_data),
      pattern = map(mcmc_data)
    ),
    
    # evaluate predictions
    # the following steps unfortunately can't be mapped directly with tar_map()
    # Claude-AI proposed some work-arounds but they seemed overly complex for
    # the little we need here; if we add more and more models, this workflow 
    # would need revision but with 3 models it's still okay?
    tar_target(preds_linreg,
               predict_linreg(mcmc_linreg, mcmc_data),
               pattern = map(mcmc_linreg, mcmc_data)),
    tar_target(preds_eivreg1,
               predict_eivreg(mcmc_eivreg_known_sdmex, mcmc_data),
               pattern = map(mcmc_eivreg_known_sdmex, mcmc_data)),
    tar_target(preds_eivreg2,
               predict_eivreg(mcmc_eivreg_unknown_sdmex, mcmc_data),
               pattern = map(mcmc_eivreg_unknown_sdmex, mcmc_data)),
    tar_target(
      predeval,
      eval_preds(preds_linreg, preds_eivreg1, preds_eivreg2)
    ),
    # MCMC-diagnostics
    tar_target(mcmcdx_linreg, 
               mcmc_dx(mcmc_linreg), 
               pattern = map(mcmc_linreg)),
    tar_target(mcmcdx_eivreg1, 
               mcmc_dx(mcmc_eivreg_known_sdmex), 
               pattern = map(mcmc_eivreg_known_sdmex)),
    tar_target(mcmcdx_eivreg2, 
               mcmc_dx(mcmc_eivreg_unknown_sdmex), 
               pattern = map(mcmc_eivreg_unknown_sdmex))
  ),
  
  # combine results across scenario's
  tar_combine(
    predeval_summary,
    mapped[["predeval"]],
    command = bind_rows(!!!.x, .id = "scenario") %>%
      tidy_predeval_summary()
  ),
  tar_combine(
    regdilution_summary,
    mapped[["regdilution"]],
    command = bind_rows(!!!.x)
  ),
  tar_combine(
    mcmcdx_linreg_all,
    mapped[["mcmcdx_linreg"]],
    command = bind_rows(!!!.x, .id = "scenario") %>%
      tidy_mcmcdx()
  ),
  tar_combine(
    mcmcdx_eivreg1_all,
    mapped[["mcmcdx_eivreg1"]],
    command = bind_rows(!!!.x, .id = "scenario") %>%
      tidy_mcmcdx()
  ),
  tar_combine(
    mcmcdx_eivreg2_all,
    mapped[["mcmcdx_eivreg2"]],
    command = bind_rows(!!!.x, .id = "scenario") %>%
      tidy_mcmcdx()
  )
  
)
