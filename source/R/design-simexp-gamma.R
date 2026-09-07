# design-simexp-gamma.R
# design simulation experiments with gamma-distributed variables


# parameters of the scenario's  (first = default value):
# should match the arguments of the function sim_data_gamma()
params <- list(
  sample_size          = c(200, 100, 500),
  ratio_cvmex_cvx      = c(0.05, 0.01, 0.10, 0.20),
  ratio_mey_mex        = c(1, 0.9, 1.1),
  corr_error           = c(0, 0.5, 0.9),
  mu_x                 = 1,
  cv_x                 = .5,
  alpha                = c(0, -.5, .5),
  beta                 = c(1, 0.7, 1.3),
  cv_y                 = c(0.05, 0.01, 0.10, 0.20),
  ratio_cvme_val_train = c(1, 0.8, 1.2, 1.5)
)
defaults <- lapply(params, `[`, 1)

# one block per parameter: vary that parameter, keep others at default
df_list <- lapply(names(params), function(pname) {
  # only for parameters with 2+ values
  nvals <- length(params[[pname]])
  if (nvals >= 2) {
    df <- as.data.frame(defaults, stringsAsFactors = FALSE)
    df <- df[rep(1, nvals), ] # repeat default row
    df[[pname]] <- params[[pname]] # overwrite with variations
    df$varying[rep(1, nvals)] <- pname
    df
  }
})

scenarios <- do.call(rbind, df_list)
rownames(scenarios) <- NULL

# provide labels for targets pipeline
scenarios <- scenarios %>% 
  mutate(
    scenario_label = case_when(
      varying == "sample_size" ~ paste0("Nsample", sample_size),
      varying == "ratio_cvmex_cvx" ~ paste0("Taux", ratio_cvmex_cvx),
      varying == "ratio_mey_mex" ~ paste0("Tauxy", ratio_mey_mex),
      varying == "corr_error" ~ paste0("CorrME", corr_error),
      varying == "mu_x" ~ paste0("Mux", mu_x),
      varying == "cv_x" ~ paste0("CVx", cv_x),
      varying == "alpha" ~ paste0("Alpha", alpha),
      varying == "beta" ~ paste0("Beta", beta),
      varying == "cv_y" ~ paste0("CVy", cv_y),
      varying == "ratio_cvme_val_train" ~ paste0("RatioCVme",
                                                 ratio_cvme_val_train)
    )
  )

rm(params, defaults, df_list)

