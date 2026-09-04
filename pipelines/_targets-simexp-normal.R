# _targets-simexp-normal.R
library(targets)
library(tarchetypes)
library(stantargets)
library(posterior) |> suppressPackageStartupMessages()
library(tidyverse) |> suppressPackageStartupMessages()
library(here) |> suppressPackageStartupMessages()
library(quarto)
library(crew)

# Pre-compile the models sequentially
cmdstanr::cmdstan_model(here("source/stan-normal/linreg.stan"))
cmdstanr::cmdstan_model(here("source/stan-normal/eivreg_known_sdmex.stan"))
cmdstanr::cmdstan_model(here("source/stan-normal/eivreg_unknown_sdmex.stan"))

tar_option_set(
  controller = crew_controller_local(workers = 5)
)
tar_source(
  files = c(
    here("source/R/design-simexp-normal.R"),
    here("source/R/simulate-data-normal.R"),
    here("source/R/predictions-normal.R"),
    here("source/R/evaluate-predictions.R"),
    here("source/R/mcmc_dx.R"),
    here("source/R/utils-normal.R")
  )
)

scenario_label <- scenarios$scenario_label


### ---------------- ###
### Targets-pipeline ###
### ---------------- ###
list(
  # part of the pipeline to map over scenario's:
  mapped <- tar_map(
    unlist = FALSE,
    values = scenarios,
    names  = scenario_label,
    
    # for each batch/rep: draw simulated data and fit Bayesian models on it
    # the function sim_data returns both a training and a validation dataset
    tar_stan_mcmc_rep_draws(
      name       = mcmc,
      stan_files = c(here("source/stan-normal/linreg.stan"),
                     here("source/stan-normal/eivreg_known_sdmex.stan"),
                     here("source/stan-normal/eivreg_unknown_sdmex.stan")),
      data = sim_data_normal(
        sample_size             = sample_size,
        ratio_sdmex_sigmax      = ratio_sdmex_sigmax,
        ratio_sdmey_sdmex       = ratio_sdmey_sdmex,
        corr_sdmey_sdmex        = corr_sdmey_sdmex,
        tails                   = tails,
        mu_x                    = mu_x,
        sigma_x                 = sigma_x,
        alpha                   = alpha,
        beta                    = beta,
        sigma_y_struct          = sigma_y_struct,
        ratio_sdmeval_sdmetrain = ratio_sdmeval_sdmetrain
      ),
      seed          = 123,
      chains        = 4, parallel_chains = 4,
      iter_warmup   = 1000,
      iter_sampling = 1000,
      refresh       = 0,
      batches       = 5,
      reps          = 2,
      stdout = R.utils::nullfile(),
      stderr = R.utils::nullfile()
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
               predict_eivreg_knownsd(mcmc_eivreg_known_sdmex, mcmc_data),
               pattern = map(mcmc_eivreg_known_sdmex, mcmc_data)),
    tar_target(preds_eivreg2,
               predict_eivreg(mcmc_eivreg_unknown_sdmex, mcmc_data),
               pattern = map(mcmc_eivreg_unknown_sdmex, mcmc_data)),
    tar_target(
      predeval,
      eval_preds(preds_linreg, preds_eivreg1, preds_eivreg2)
    ),
    # paired ELPD model comparison (see compare_models() in
    # evaluate-predictions.R)
    tar_target(
      predcompare,
      compare_models(preds_linreg, preds_eivreg1, preds_eivreg2)
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
               pattern = map(mcmc_eivreg_unknown_sdmex)),
    tar_target(
      mcmcdx,
      combine_mcmcdx(mcmcdx_linreg, mcmcdx_eivreg1, mcmcdx_eivreg2)
    )
  ),
  
  # combine results across scenario's
  tar_combine(
    predeval_summary,
    mapped[["predeval"]],
    command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_normal()
  ),
  tar_combine(
    predcompare_summary,
    mapped[["predcompare"]],
    command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_normal()
  ),
  tar_combine(
    mcmcdx_summary,
    mapped[["mcmcdx"]],
    command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_normal()
  ),
  
  # render a quarto report of the experiment
  tar_quarto(
    report_simexp1,
    path = here("source/quarto/analysis-simexp-normal.qmd")
  )
  
)
