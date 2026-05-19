# design of the simulation experiment


# sim_data function:
source("simulate-data.r")


# parameter choices of the simulation experiment
# should match the arguments of the function sim_data
sample_size        <- c(100, 500) # c(100, 200, 500)
ratio_sdmex_sigmax <- c(0.05, 0.1, 0.2)
ratio_sdmey_sdmex  <- 1 # c(0.9, 1.0, 1.1)
tails              <- c("normal", "t_df3")
replicates         <- 2

simexp_design <- expand_grid(
  sample_size,
  ratio_sdmex_sigmax,
  tails,
  rep = seq(1, replicates)
)

simexp_data <- simexp_design %>%
  mutate(
    data_train = pmap(
      .f = sim_data,
      .l = list(N                  = sample_size,
                ratio_sdmex_sigmax = ratio_sdmex_sigmax,
                tails              = tails)
    ),
    data_test = pmap(
      .f = sim_data,
      .l = list(N                  = 1e3,
                ratio_sdmex_sigmax = ratio_sdmex_sigmax,
                tails              = tails)
    )
  )

# expand design matrix with list of models
simexp_data <- simexp_data %>%
  expand_grid(models = c("linreg","eivreg1","eivreg2"))

save(simexp_data,
     file = "simulation-data/simexp-data.rda")


