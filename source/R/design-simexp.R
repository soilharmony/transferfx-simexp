# design of the simulation experiment

# parameter choices of the simulation experiment
# should match the arguments of the function sim_data
simexp_design <- expand_grid(
  sample_size        = c(100, 200, 500),
  ratio_sdmex_sigmax = c(0.05, 0.1, 0.2),
  ratio_sdmey_sdmex  = 1,
  tails              = c("normal", "tdf3")
) %>%
  mutate(
    sample_size_label        = paste0("Nsample", sample_size), 
    ratio_sdmex_sigmax_label = paste0("Tau", ratio_sdmex_sigmax),
    tails_label              = paste0("Tail",tails)
  )
