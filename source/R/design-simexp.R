# design of the simulation experiment


# parameters of the scenario's:
# should match the arguments of the function sim_data()
#   sample_size
#   ratio_sdmex_sigmax
#   ratio_sdmey_sdmex
#   corr_sdmey_sdmex
#   tails
#   mu_x
#   sigma_x
#   alpha
#   beta
#   sigma_y_struct


scenario_labeller <- function(df) {
  df %>% 
    mutate(
      sample_size_label        = paste0("Nsample", sample_size), 
      ratio_sdmex_sigmax_label = paste0("Taux", ratio_sdmex_sigmax),
      ratio_sdmey_sdmex_label  = paste0("Tauxy", ratio_sdmey_sdmex),
      corr_sdmey_sdmex_label   = paste0("CorrME", corr_sdmey_sdmex),
      tails_label              = paste0("Tail",tails),
      mu_x_label               = paste0("Mux", mu_x),
      sigma_x_label            = paste0("Sigmax", sigma_x),
      alpha_label              = paste0("Alpha", alpha),
      beta_label               = paste0("Beta", beta),
      sigma_y_struct_label     = paste0("Sigmay", sigma_y_struct)
    )
}



#' varying parameters for experiment 1: 
#' Q: what is the impact of the sample size, the tails of the measurement error 
#' and the ratio between the SD of the latent X and the measurement error on X?
#' * sample_size + ratio_sdmex_sigmax + tails *
#' 
simexp_design1 <- expand_grid(
  sample_size        = c(100, 200, 500),
  ratio_sdmex_sigmax = c(0.01, 0.1, 0.2),
  ratio_sdmey_sdmex  = 1,
  corr_sdmey_sdmex   = 0,
  tails              = c("normal", "tdf3"),
  mu_x               = 0,
  sigma_x            = 1,
  alpha              = 0,
  beta               = 1,
  sigma_y_struct     = .1
) %>% scenario_labeller()


#' varying parameters for experiment 2: 
#' Q: what is the impact of the structural residual variance between the latent
#' values (sigma_y_struct) and the correlation between the measurement errors?
#' * sigma_y_struct + corr_sdmey_sdmex *
#' 
simexp_design2 <- expand_grid(
  sample_size        = 200,
  ratio_sdmex_sigmax = 0.1,
  ratio_sdmey_sdmex  = 1,
  corr_sdmey_sdmex   = c(0.0, 0.5, 0.9),
  tails              = "normal",
  mu_x               = 0,
  sigma_x            = 1,
  alpha              = 0,
  beta               = 1,
  sigma_y_struct     = c(.01, .1, .2)
) %>% scenario_labeller()


#' varying parameters for experiment 3: 
#' Q: when we keep our priors on alpha and beta fixed, but the true values
#' are not 0 and 1, does it hurt our predictions?
#' * alpha + beta *
#' 
simexp_design3 <- expand_grid(
  sample_size        = 200,
  ratio_sdmex_sigmax = 0.1,
  ratio_sdmey_sdmex  = 1,
  corr_sdmey_sdmex   = 0.0,
  tails              = "normal",
  mu_x               = 0,
  sigma_x            = 1,
  alpha              = c(-0.5, 0, 0.5),
  beta               = c(0.7, 1, 1.3),
  sigma_y_struct     = .1
) %>% scenario_labeller()


