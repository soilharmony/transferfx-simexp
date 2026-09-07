# _targets-simexp-gamma.R
library(targets)
library(tarchetypes)
library(stantargets)
library(posterior) |> suppressPackageStartupMessages()
library(tidyverse) |> suppressPackageStartupMessages()
library(here) |> suppressPackageStartupMessages()
library(quarto)
library(crew)

# Pre-compile the models sequentially
cmdstanr::cmdstan_model(here("source/stan-gamma/gamma_linreg.stan"))
cmdstanr::cmdstan_model(here("source/stan-gamma/gamma_linreglogtrafo.stan"))
cmdstanr::cmdstan_model(here("source/stan-gamma/gammareg.stan"))
cmdstanr::cmdstan_model(here("source/stan-gamma/gammareg_eiv_knowncvmex.stan"))
cmdstanr::cmdstan_model(here("source/stan-gamma/gammareg_eiv_unknowncvmex.stan"))

tar_option_set(
  controller = crew_controller_local(workers = 5)
)
tar_source(
  files = c(
    here("source/R/design-simexp-gamma.R"),
    here("source/R/simulate-data-gamma.R"),
    here("source/R/predictions-gamma.R"),
    here("source/R/evaluate-predictions-gamma.R"),
    here("source/R/mcmc_dx.R"),
    here("source/R/utils-gamma.R")
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
        here("source/stan-gamma/gamma_linreg.stan"),
        here("source/stan-gamma/gamma_linreglogtrafo.stan"),
        here("source/stan-gamma/gammareg.stan"),
        here("source/stan-gamma/gammareg_eiv_knowncvmex.stan"),
        here("source/stan-gamma/gammareg_eiv_unknowncvmex.stan"),
      ),
      data = sim_data_gamma(
        sample_size          = sample_size,
        mu_x                 = mu_x,      
        cv_x                 = cv_x, 
        alpha                = alpha,
        beta                 = beta,
        cv_y                 = cv_y,
        ratio_cvmex_cvx      = ratio_cvmex_cvx,
        ratio_mey_mex        = ratio_mey_mex,
        corr_error           = corr_error,
        ratio_cvme_val_train = ratio_cvme_val_train
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
    
    # get prediction summaries per model
    tar_target(
      preds_linreg,
      summarise_predictions(mcmc_gamma_linreg, mcmc_data, 
                            family = "normal"),
      pattern = map(mcmc_gamma_linreg, mcmc_data)
    ),
    tar_target(
      preds_linreglogtrafo,
      summarise_predictions(mcmc_gamma_linreglogtrafo, mcmc_data, 
                            family = "lognormal"),
      pattern = map(mcmc_gamma_linreglogtrafo, mcmc_data)
    ),
    tar_target(
      preds_gammareg,
      summarise_predictions(mcmc_gammareg, mcmc_data,
                            family = "gamma"),
      pattern = map(mcmc_gammareg, mcmc_data)
    ),
    tar_target(
      preds_gammaregeivknowncvmex,
      summarise_predictions(mcmc_gammareg_eiv_knowncvmex, mcmc_data,
                            family = "gamma"),
      pattern = map(mcmc_gammareg_eiv_knowncvmex, mcmc_data)
    ),
    tar_target(
      preds_gammaregeivunknowncvmex,
      summarise_predictions(mcmc_gammareg_eiv_unknowncvmex, mcmc_data,
                            family = "gamma"),
      pattern = map(mcmc_gammareg_eiv_unknowncvmex, mcmc_data)
    ),
    
    #evaluate predictions
    tar_target(
      predeval,
      eval_preds_gamma(preds_linreg, preds_linreglogtrafo,
                       preds_gammareg, preds_gammaregeivknowncvmex,
                       preds_gammaregeivunknowncvmex)
    ),
    
    # paired ELPD model comparison (see compare_models() in
    # evaluate-predictions-gamma.R)
    tar_target(
      predcompare,
      compare_models(preds_linreg, preds_linreglogtrafo,
                     preds_gammareg, preds_gammaregeivknowncvmex,
                     preds_gammaregeivunknowncvmex)
    ),
    
    # MCMC-diagnostics
    tar_target(mcmcdx_linreg,
               mcmc_dx(mcmc_gamma_linreg),
               pattern = map(mcmc_gamma_linreg)),
    tar_target(mcmcdx_linreglogtrafo,
               mcmc_dx(mcmc_gamma_linreglogtrafo),
               pattern = map(mcmc_gamma_linreglogtrafo)),
    tar_target(mcmcdx_gammareg,
               mcmc_dx(mcmc_gammareg),
               pattern = map(mcmc_gammareg)),
    tar_target(mcmcdx_gammaregeivknowncvmex,
               mcmc_dx(mcmc_gammareg_eiv_knowncvmex),
               pattern = map(mcmc_gammareg_eiv_knowncvmex)),
    tar_target(mcmcdx_gammaregeivunknowncvmex,
               mcmc_dx(mcmc_gammareg_eiv_unknowncvmex),
               pattern = map(mcmc_gammareg_eiv_unknowncvmex)),
    tar_target(
      mcmcdx,
      combine_mcmcdx(mcmcdx_linreg, mcmcdx_linreglogtrafo, mcmcdx_gammareg,
                     mcmcdx_gammaregeivknowncvmex, 
                     mcmcdx_gammaregeivunknowncvmex)
    )
  ),
  
  # combine results across scenario's
  tar_combine(
    predeval_summary,
    mapped[["predeval"]],
    command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_gamma()
  ),
  tar_combine(
    predcompare_summary,
    mapped[["predcompare"]],
    command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_gamma()
  ),
  tar_combine(
    mcmcdx_summary,
    mapped[["mcmcdx"]],
    command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_gamma()
  )
  
  # render a quarto report of the experiment
  # tar_quarto(
  #   report_simexp,
  #   path = here("source/quarto/analysis-simexp-gamma.qmd")
  # )
  
)
