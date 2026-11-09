# _targets-simexp-doublesampling.R
library(targets)
library(tarchetypes)
library(posterior) |> suppressPackageStartupMessages()
library(tidyverse) |> suppressPackageStartupMessages()
library(here) |> suppressPackageStartupMessages()
library(quarto)
library(crew)

tar_option_set(
  controller = crew_controller_local(workers = 5)
)
tar_source(
  files = c(
    here("source/R/design-simexp-doublesampling.R"),
    here("source/R/simulate-data-doublesampling.R"),
    here("source/R/predictions-doublesampling.R")
  )
)

nbatch <- 5
nrep   <- 50

### ---------------- ###
### Targets-pipeline ###
### ---------------- ###
list(
  # part of the pipeline to map over scenario's:
  tar_map_rep(
    name    = simulated_data_calibration,
    unlist  = FALSE,
    values  = scenarios,
    names   = any_of("scenario_label"),
    batches = nbatch,
    reps    = nrep,
    command = sim_data_doublesampling(
      sample_size           = sample_size,
      mu_x                  = mu_x,   
      sigma2_pA             = sigma2_pA,   
      sigma2_aA1            = sigma2_aA1,
      ratio_area1_area2     = ratio_area1_area2,
      n_subsamples          = n_subsamples,
      ratio_n1_n2           = ratio_n1_n2,
      spatautocor           = spatautocor,
      corr_latent           = corr_latent,
      corr_mefield          = corr_mefield,
      beta0                 = beta0, 
      beta1                 = beta1,
      ratio_labmex_fieldmex = ratio_labmex_fieldmex,
      ratio_labmey_labmex   = ratio_labmey_labmex,
      corr_melab            = corr_melab
    )
  ),
  
  tar_map_rep(
    name    = simulated_data_validation,
    unlist  = FALSE,
    values  = scenarios,
    names   = any_of("scenario_label"),
    batches = nbatch,
    reps    = nrep,
    command = sim_data_doublesampling(
      sample_size           = 1e3,
      mu_x                  = mu_x,   
      sigma2_pA             = sigma2_pA,   
      sigma2_aA1            = sigma2_aA1,
      ratio_area1_area2     = ratio_area1_area2,
      n_subsamples          = n_subsamples,
      ratio_n1_n2           = ratio_n1_n2,
      spatautocor           = spatautocor,
      corr_latent           = corr_latent,
      corr_mefield          = corr_mefield,
      beta0                 = beta0, 
      beta1                 = beta1,
      ratio_labmex_fieldmex = ratio_labmex_fieldmex,
      ratio_labmey_labmex   = ratio_labmey_labmex,
      corr_melab            = corr_melab
    )
  ),
  
  tar_target(
    prediction,
    predict_eval_doublesampling(
      simulated_data_calibration, simulated_data_validation
    )
  ),
  
  # render a quarto report of the experiment
  tar_quarto(
    report_simexp,
    path = here("source/quarto/analysis-simexp-doublesampling.qmd")
  )
  
)
