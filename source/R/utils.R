# utility functions





#' @title tidy_scenario_normal
#' @description
#' Extract scenario parameters from the scenario column
#' @param df ....
tidy_scenario_normal <- function(df) {
browser()
  df %>%
    separate_wider_delim(
      scenario, "_", 
      names = c("x","sample_size","taux","tauxy","corrme",
                "tails","mux","sigmax","alpha","beta","sigmay_struct",
                "ratio_sdme")
    ) %>%
    mutate(
      alpha = case_when(alpha == "Alpha.0.5" ~ "Alpha-0.5", TRUE ~ alpha),
      across(c(sample_size, taux, tauxy, corrme, 
               mux, sigmax, alpha, beta, sigmay_struct, ratio_sdme), 
             ~ as.numeric(gsub("[^0-9.-]", "", .x))),
      tails = substr(tails, 5, nchar(tails))
    ) %>%
    select(-x)
}


#' @title tidy_scenario_gamma
#' @description
#' Extract scenario parameters from the scenario column
#' @param df ....
tidy_scenario_gamma <- function(df) {
  df %>%
    separate_wider_delim(
      scenario, "_", 
      names = c("x","sample_size","mux","cvx","alpha","beta",
                "cvy","taux","tauxy","corr_mexy","ratio_cvme")
    ) %>%
    mutate(
      alpha = case_when(alpha == "Alpha.0.5" ~ "Alpha-0.5",
                        alpha == "Alpha.0.2" ~ "Alpha-0.2",
                        TRUE ~ alpha),
      across(c(sample_size, mux, cvx, alpha, beta, 
               cvy, taux, tauxy, corr_mexy, ratio_cvme), 
             ~ as.numeric(gsub("[^0-9.-]", "", .x)))
    ) %>%
    select(-x)
}


