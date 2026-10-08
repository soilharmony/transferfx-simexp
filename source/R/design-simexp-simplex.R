# design-simexp-simplex.R
# 
# design simulation experiments with 3d-simplex-distributed variables
# clay + silt + sand = 1


# parameters of the scenario's  (first = default value):
# should match the arguments of the function sim_data_simplex()
params <- list(
  sample_size           = c(200, 100, 500),
  alpha_ilr             = c(0, -0.5, 0.5),  
  beta_ilr              = c(1, 0.7, 1.3), 
  sigma_ilr             = c(.1, .01, .2), 
  ratio_sdmex_sdx       = c(0.1, 0.01, 0.2),
  ratio_sdmey_sdmex     = c(1, 0.9, 1.1),
  corr_mexy             = c(0, 0.5, 0.9),
  ratio_sdmeval_sdmecal = c(1, 0.8, 1.2, 1.5)
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

# add mu-vector and cov-matrix to scenarios data.frame (kept fixed)
scenarios$mu_ilr  <- rep(list(c(.5, .25)), nrow(scenarios))
scenarios$cov_ilr <- rep(list(matrix(c(.16, -.14, -.14, 1.34), 
                                     byrow=T, nrow=2)), nrow(scenarios))


# provide labels for targets pipeline
scenarios <- scenarios %>%
  relocate(c(mu_ilr, cov_ilr), .before = varying) %>%
  mutate(
    scenario_label = case_when(
      varying == "sample_size" ~ paste0("Nsample", sample_size),
      varying == "ratio_sdmex_sdx" ~ paste0("Taux", ratio_sdmex_sdx),
      varying == "ratio_sdmey_sdmex" ~ paste0("Tauxy", ratio_sdmey_sdmex),
      varying == "corr_mexy" ~ paste0("CorrME", corr_mexy),
      varying == "alpha_ilr" ~ paste0("Alpha", alpha_ilr),
      varying == "beta_ilr" ~ paste0("Beta", beta_ilr),
      varying == "sigma_ilr" ~ paste0("SDy", sigma_ilr),
      varying == "ratio_sdmeval_sdmecal" ~ paste0("RatioSDme",
                                                  ratio_sdmeval_sdmecal)
    )
  )

rm(params, defaults, df_list)

