# simulate-data-normal-doublesampling.R

# new interpretation of double sampling strategy
# instead of working with split samples, they will work with two samples

# convention:
# field protocol 1 (P1) + reference lab method (Y)
# field protocol 2 (P2) + non-reference lab method (X)

# sim_data_normal_ds
sim_data_normal_ds <- function(
    # number of locations (supports) to be double-sampled
    sample_size = 200,
    
    # parameters for the field measurements
    mu_x              = 0,   # mean in the population
    sigma2_pA         = 1,   # point-support variance in the population
    sigma2_aA1        = .9,  # between area-support variance (P1)
    ratio_area1_area2 = 1,   # ratio of support area's (area1 / area2)
    n_subsamples      = 5,   # number of sub-samples within the support (P1)
    ratio_n1_n2       = 1,   # ratio of number of sub-samples (n1 / n2)
    spatautocor       = .5,  # spatial autocorrelation
    corr_latent       = .99, # correlation between latent averages of supports
                             # < 1 because supports not 100% overlapping
    corr_mefield      = 0,   # correlation field measurement err. 
                             # (eg. same point-locations in both protocols)
    
    # parameters for the lab measurements
    beta0                 = 0,   # transfer-function intercept
    beta1                 = 1,   # transfer-function slope
    ratio_labmex_fieldmex = .05, # ratio of SD measurement err. lab / field
    ratio_labmey_labmex   = 1,   # ratio of SD lab measurement err. y / x
    corr_melab            = 0,   # correlation lab measurement err.
                                 # (eg. shared pre-processing procedure) 
                                 # still necessary ????????
                                 # only correlated for split samples?
    
    # ratio between SD of measurement error between calibration and validation
    ratio_sdmeval_sdmetrain = 1
) {
  N <- sample_size
  
  ##### (1) field protocol #####
  
  # variance of support-average values (protocol 1)
  sigma2_support1 <- sigma2_aA1
  
  # variance of support-average values (protocol 2)
  R_a             <- ratio_area1_area2^spatautocor
  sigma2_support2 <- sigma2_support1 * R_a
  
  # variance of within-support point values (Krige's relation)
  sigma2_wpoint1 <- sigma2_pA - sigma2_support1
  sigma2_wpoint2 <- sigma2_pA - sigma2_support2
  
  # sampling variance from taking n sub-samples within the support:
  n_subsamples2    <- round(n_subsamples / ratio_n1_n2)
  sigma2_sampling1 <- sigma2_wpoint1 / n_subsamples
  sigma2_sampling2 <- sigma2_wpoint2 / n_subsamples2
  
  # latent support-average values with both protocols (non-reference method)
  var_latent <- matrix(
    c(sigma2_support1,
      rep(sqrt(sigma2_support1 * sigma2_support2) * corr_latent, 2), 
      sigma2_support2),
    byrow = TRUE, nrow = 2
  )
  latent_x <- MASS::mvrnorm(N, rep(mu_x, 2), var_latent)
  
  # TF from the non-reference method to the reference method with both protocols
  latent_y <- beta0 + beta1 * latent_x

  # field sub-sampling error (approximating support mean with n sub-samples)
  varme_field <- matrix(
    c(sigma2_sampling1, 
      rep(sqrt(sigma2_sampling1 * sigma2_sampling2) * corr_mefield, 2), 
      sigma2_sampling2),
    byrow = TRUE, nrow = 2
  )
  err_field   <- MASS::mvrnorm(N, rep(0, 2), varme_field)
  obs_field_x <- latent_x + err_field
  obs_field_y <- latent_y + err_field
  # obs_field_% is what we would observe if there was no lab measurement error
  
  
  ##### (2) lab method #####
  sd_lab_mex <- ratio_labmex_fieldmex * sqrt(sigma2_sampling2)
  sd_lab_mey <- ratio_labmey_labmex * sd_lab_mex
  varme_lab  <- matrix(
    c(sd_lab_mex^2, rep(sd_lab_mex * sd_lab_mey * corr_melab, 2), sd_lab_mey^2),
    byrow = TRUE, nrow = 2
  )
  err_lab   <- MASS::mvrnorm(N, rep(0, 2), varme_lab)
  obs_lab_x <- obs_field_x + err_lab
  obs_lab_y <- obs_field_y + err_lab
  
  
  ##### (3) mix field protocol & lab method #####
  latent   <- cbind(latent_x[, 2], latent_y[, 1])
  observed <- cbind(obs_lab_x[, 2], obs_lab_y[, 1])
  
  list_stan <- list(
    latent_x    = data.frame(latent_x),
    latent_y    = data.frame(latent_y),
    obs_field_x = data.frame(obs_field_x),
    obs_field_y = data.frame(obs_field_y),
    obs_lab_x   = data.frame(obs_lab_x),
    obs_lab_y   = data.frame(obs_lab_y),
    latent      = data.frame(latent),
    observed    = data.frame(observed)
  )
  return(list_stan)
}

