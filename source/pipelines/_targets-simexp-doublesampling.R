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
    here("source/R/simulate-data-doublesampling.R")
    #here("source/R/predictions-doublesampling.R"),
    #here("source/R/utils-doublesampling.R")
  )
)

### ---------------- ###
### Targets-pipeline ###
### ---------------- ###
list(
  # part of the pipeline to map over scenario's:
  mapped <- tar_map_rep(
    unlist  = FALSE,
    values  = scenarios,
    names   = "scenario_label",
    batches = 5,
    reps    = 10,
    
    # the function sim_data returns both a training and a validation dataset
    tar_target(
      simulated_data,
      sim_data_doublesampling(
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
    )
  )
  
  # combine results across scenario's
  # tar_combine(
  #   predeval_summary,
  #   mapped[["predeval"]],
  #   command = bind_rows(!!!.x, .id = "scenario") %>% tidy_scenario_simplex()
  # ),
  
  # render a quarto report of the experiment
  # tar_quarto(
  #   report_simexp,
  #   path = here("source/quarto/analysis-simexp-doublesampling.qmd")
  # )
  
)
