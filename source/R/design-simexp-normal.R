# design-simexp-normal.R
# design simulation experiments with normal distributed variables



# parameters of the scenario's  (first = default value):
# should match the arguments of the function sim_data_normal()
params <- list(
  sample_size             = c(200, 100, 500),
  ratio_sdmex_sigmax      = c(0.1, 0.01, 0.2),
  ratio_sdmey_sdmex       = c(1, 0.9, 1.1),
  corr_sdmey_sdmex        = c(0, 0.5, 0.9),
  tails                   = c("normal", "tdf3"),
  mu_x                    = 0,
  sigma_x                 = 1,
  alpha                   = c(0, -0.5, 0.5),
  beta                    = c(1, 0.7, 1.3),
  sigma_y_struct          = c(.1, .01, .2),
  ratio_sdmeval_sdmetrain = c(1, 0.8, 1.2, 1.5)
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
      varying == "ratio_sdmex_sigmax" ~ paste0("Taux", ratio_sdmex_sigmax),
      varying == "ratio_sdmey_sdmex" ~ paste0("Tauxy", ratio_sdmey_sdmex),
      varying == "corr_sdmey_sdmex" ~ paste0("CorrME", corr_sdmey_sdmex),
      varying == "tails" ~ paste0("Tail",tails),
      varying == "mu_x" ~ paste0("Mux", mu_x),
      varying == "sigma_x" ~ paste0("Sigmax", sigma_x),
      varying == "alpha" ~ paste0("Alpha", alpha),
      varying == "beta" ~ paste0("Beta", beta),
      varying == "sigma_y_struct" ~ paste0("Sigmay", sigma_y_struct),
      varying == "ratio_sdmeval_sdmetrain" ~ paste0("RatioSDme",
                                                    ratio_sdmeval_sdmetrain)
    )
  )


rm(params, defaults, df_list)

