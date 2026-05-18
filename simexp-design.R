# design of the simulation experiment


# sim_data function:
source("simulate-data.r")


# parameter choices of the simulation experiment
# should match the arguments of the function sim_data
sample_size         <- c(100, 500) # c(100, 200, 500)
ratio_varmex_varx   <- c(0.05, 0.1, 0.2)
ratio_varmey_varmex <- 1 # c(0.9, 1.0, 1.1)
tails               <- c("normal", "t_df3")
replicates          <- 1

simexp_design <- expand_grid(
  sample_size,
  ratio_varmex_varx,
  tails,
  rep = seq(1, replicates)
)

simexp_data <- simexp_design %>%
  mutate(
    data_train = pmap(
      .f = sim_data,
      .l = list(N                 = sample_size,
                ratio_varmex_varx = ratio_varmex_varx,
                tails             = tails)
    ),
    data_test = pmap(
      .f = sim_data,
      .l = list(N                 = 1e3,
                ratio_varmex_varx = ratio_varmex_varx,
                tails             = tails)
    )
  )

save(simexp_data,
     file = "simulation-data/simexp-data.rda")


