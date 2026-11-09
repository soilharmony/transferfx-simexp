# _targets-simexp-leftcens.R
library(targets)
library(tarchetypes)
library(stantargets)
library(posterior) |> suppressPackageStartupMessages()
library(tidyverse) |> suppressPackageStartupMessages()
library(here) |> suppressPackageStartupMessages()
library(quarto)
library(crew)

# Pre-compile the models sequentially
# cmdstanr::cmdstan_model(here("source/stan-leftcens/xxxxx.stan"))

tar_option_set(
  controller = crew_controller_local(workers = 5)
)
tar_source(
  files = c(
    # here("source/R/design-simexp-leftcens.R"),
    here("source/R/simulate-data-leftcens.R")
    # here("source/R/predictions-leftcens.R"),
    # here("source/R/evaluate-predictions-leftcens.R"),
    # here("source/R/mcmc_dx.R"),
    # here("source/R/utils-leftcens.R")
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
      stan_files = c(
        #here("source/stan-simplex/mvlinreg.stan")
      ),
      data = sim_data_leftcens(

      ),
      seed          = 123,
      chains        = 4, parallel_chains = 4,
      iter_warmup   = 1000,
      iter_sampling = 1000,
      refresh       = 0,
      batches       = 5,
      reps          = 3,
      stdout = R.utils::nullfile(),
      stderr = R.utils::nullfile()
    ),
    
    # get prediction summaries per model
    tar_target(
      preds_mvlinreg,
      summarise_predictions(mcmc_mvlinreg, mcmc_data),
      pattern = map(mcmc_mvlinreg, mcmc_data)
    ),
    
    # evaluate predictions
    tar_target(
      predeval,
      eval_preds_simplex(preds_mvlinreg)
    ),
    
    # paired CRPS model comparison
    tar_target(
      predcompare,
      compare_models(preds_mvlinreg)
    ),
    
    # MCMC-diagnostics
    tar_target(mcmcdx_mvlinreg,
               mcmc_dx_simplex(mcmc_mvlinreg),
               pattern = map(mcmc_mvlinreg)),
    tar_target(
      mcmcdx,
      combine_mcmcdx(mcmcdx_mvlinreg)
    )
  ),
  
  # combine results across scenario's
  tar_combine(
    predeval_summary,
    mapped[["predeval"]],
    command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_simplex()
  ),
  tar_combine(
    predcompare_summary,
    mapped[["predcompare"]],
    command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_simplex()
  ),
  tar_combine(
    mcmcdx_summary,
    mapped[["mcmcdx"]],
    command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_simplex()
  ),
  
  # render a quarto report of the experiment
  tar_quarto(
    report_simexp,
    path = here("source/quarto/analysis-simexp-leftcens.qmd")
  )
  
)
