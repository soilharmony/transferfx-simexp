library(brms)
library(dplyr)

source("simulate-data.R")

# dataframe containing y_obs and x_obs
data <- sim_data(
  N = 100,
  ratio_sdmex_sigmax = 0.1,
  tails = "normal")

# prepare data for use with brms non-linear
d_y <- data.frame(
  response = data$y_obs,
  is_y = 1,
  xobs_var = data$x_obs
)

d_x <- data.frame(
  response = data$x_obs, # observed x
  is_y = 0,
  xobs_var = 0 # Not used in the x_obs likelihood
)

d_stacked <- bind_rows(d_y, d_x)

bform <- bf(
  # The conditional mean
  response ~
    is_y * (bIntercept + bxobs * (1 / (1 / sigmax ^ 2 + 1 / sigmamex ^ 2)) *
              (mux / sigmax ^ 2 + xobs_var / sigmamex ^ 2))
  + (1 - is_y) * mux,
  
  # The conditional standard deviation (sigma) on the log-link
  nlf(
    sigma ~
      is_y * log(sqrt(sigmares ^ 2 + bxobs ^ 2 *
                        (1 / (1 / sigmax ^ 2 + 1 / sigmamex ^ 2))))
    + (1 - is_y) * log(sqrt(sigmax ^ 2 + sigmamex ^ 2))
  ),
  
  # Declare all non-linear parameters
  bIntercept + bxobs + mux + sigmax + sigmamex + sigmares ~ 1,
  nl = TRUE
)

bpriors <- c(
  prior(normal(0, 1), nlpar = "mux"),                   # mu_x
  prior(lognormal(0, 0.5), nlpar = "sigmax", lb = 0),   # sigma_x
  prior(normal(0, 1), nlpar = "bIntercept"),            # b_Intercept
  prior(normal(1, 0.5), nlpar = "bxobs", lb = 0),       # b_x_obs
  prior(lognormal(0, 0.5), nlpar = "sigmares", lb = 0), # residual SD (sigma)
  prior(lognormal(0, 0.5), nlpar = "sigmamex", lb = 0)  # sigma_mex
)


fit <- brm(
  formula = bform,
  data = d_stacked,
  prior = bpriors,
  family = gaussian(),
  chains = 4,
  cores = 4
)

summary(fit)

# compare with:
library(readr)
source("fit-model.R")

fit2 <- fit_eiv2(data = data)
summary(fit2)

# benefit of using brm nl syntax is being able to use caching mechanism via file/file_refit arguments
# alternatively keep rstan approach but write caching functionality manually
# or use https://docs.ropensci.org/stantargets/ (maybe best approach?)
# https://docs.ropensci.org/stantargets/articles/simulation.html

