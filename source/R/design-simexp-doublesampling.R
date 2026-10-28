# design-simexp-doublesampling.R
# 
# design simulation experiments with new interpretation of double sampling


# parameters of the scenario's  (first = default value):
# should match the arguments of the function sim_data_doublesampling()
params <- list(
  sample_size           = c(200, 100, 500),
  mu_x                  = 7,  
  sigma2_pA             = 2,   
  sigma2_aA1            = 1.9,  
  ratio_area1_area2     = c(1, .7, .9, 1.1, 1.3),   
  n_subsamples          = c(5, 2, 10, 20),  
  ratio_n1_n2           = c(1, .8, 1.2), 
  spatautocor           = c(.1, 0.05, 0.2),  
  corr_latent           = c(.90, .99),
  corr_mefield          = c(0, 0.1, 0.3),
  beta0                 = c(0, 0.1, 0.2),  
  beta1                 = c(1, 0.7, 0.9, 1.1, 1.3), 
  ratio_labmex_fieldmex = c(.05, .01, 0.1, 0.2), 
  ratio_labmey_labmex   = 1, 
  corr_melab            = 0
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
      varying == "ratio_area1_area2" ~ paste0("RatioArea", ratio_area1_area2),
      varying == "n_subsamples" ~ paste0("Nsubsamples", n_subsamples),
      varying == "ratio_n1_n2" ~ paste0("RatioSubsamples", ratio_n1_n2),
      varying == "spatautocor" ~ paste0("SpatAutocor", spatautocor),
      varying == "corr_latent" ~ paste0("CorrLatent", corr_latent),
      varying == "corr_mefield" ~ paste0("CorrMEfield", corr_mefield),
      varying == "beta0" ~ paste0("Intercept", beta0),
      varying == "beta1" ~ paste0("Slope", beta1),
      varying == "ratio_labmex_fieldmex" ~ paste0("RatioLabField", ratio_labmex_fieldmex)
    )
  )

rm(params, defaults, df_list)

