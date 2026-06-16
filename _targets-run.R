# _targets-run.R

rstudioapi::restartSession()

library(targets)
library(tarchetypes)
library(tidyverse) |> suppressPackageStartupMessages()


tar_visnetwork(physics = TRUE, targets_only = TRUE)

tar_make()
#tar_make(callr_function = NULL, use_crew = FALSE, as_job = FALSE)

tar_meta(fields = warnings, complete_only = TRUE) %>% View()

tar_manifest() %>% View()

tar_prune()

tar_objects()

tar_load(validation_data_Nsample100_Tau0.1_Tailnormal)
tar_load(mcmc_linreg_Nsample100_Tau0.1_Tailtdf3_57d8e1a54d4fe67a)
tar_load(preds_linreg_Nsample100_Tau0.1_Tailtdf3_e68a127e7b9b3ee1)
tar_load(mcmc_eivreg_unknown_sdmex_Nsample100_Tau0.1_Tailnormal_39374aedf15d3f81)
tar_load(predeval_summary)
tar_load(mcmc_dx_Nsample100_Tau0.1_Tailnormal_8d7f259f2c1dedb2)
tar_load(mcmcdx_linreg_Nsample100_Tau0.1_Tailnormal_8d7f259f2c1dedb2)
tar_load(mcmcdx_eivreg2_Nsample100_Tau0.1_Tailnormal_675a26fae5875c3b)
tar_load(mcmc_data_Nsample200_Tau0.05_Tailtdf3_43038197ce508806)
tar_load(regdilution_Nsample100_Tau0.2_Tailtdf3_61c049d42f2cb6a4)
tar_load(regdilution_summary)
tar_load(mcmc_data_Nsample100_Tau0.1_Tailtdf3)
tar_load(mcmcdx_linreg_Nsample100_Tau0.1_Tailnormal_8d7f259f2c1dedb2)
tar_load(mcmcdx_linreg_all)

