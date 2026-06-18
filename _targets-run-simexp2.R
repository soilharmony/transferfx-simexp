# _targets-run-simexp2.R

rstudioapi::restartSession()

Sys.setenv(TAR_PROJECT = "project_simexp2")

library(targets)
library(tarchetypes)
library(tidyverse) |> suppressPackageStartupMessages()

tar_visnetwork()
#tar_visnetwork(physics = TRUE, targets_only = TRUE)

tar_make()
#tar_make(callr_function = NULL, use_crew = FALSE, as_job = FALSE)

# total runtime of the pipeline (per target?)
# ??????????????????????

tar_meta(fields = warnings, complete_only = TRUE) %>% View()

tar_manifest() %>% View()

tar_prune()

tar_objects()


tar_load(predeval_summary)
tar_load(mcmcdx_summary)
