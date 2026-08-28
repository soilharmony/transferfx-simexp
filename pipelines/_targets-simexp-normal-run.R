# _targets-simexp-normal-run.R
# interactive run of the pipeline

rstudioapi::restartSession()

Sys.setenv(TAR_PROJECT = "project_simexp_normal")

library(targets)
library(tarchetypes)
library(tidyverse) |> suppressPackageStartupMessages()

tar_visnetwork()
#tar_visnetwork(physics = TRUE, targets_only = TRUE)

tar_make(use_crew = TRUE, as_job = TRUE)
#tar_make(callr_function = NULL, use_crew = FALSE, as_job = FALSE)

# total runtime of the pipeline (per target?)
# ??????????????????????

tar_meta(fields = warnings, complete_only = TRUE) %>% View()

tar_manifest() %>% View()

tar_prune_list()
tar_prune()

tar_objects()



# file management: move intermediate targets to Google Drive
source(here::here("source/R/file-management.R"))
obj2keep <- c("predeval_summary","mcmcdx_summary")

move_targets_to_gdrive(
  tar_path_store(),
  obj2keep,
  "G:/Mijn Drive/data/SoilHarmony/simexp_tf"
)
