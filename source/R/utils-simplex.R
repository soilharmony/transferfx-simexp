# utility functions (simexp_simplex)


#' @title tidy_scenario_simplex
#' @description
#' Extract scenario parameters from the scenario column
#' @param df ....
tidy_scenario_simplex <- function(df) {
  
  # add more scenarios if necessary
  df_scenarios <- data.frame(
    scenario = c("Nsample200", "Nsample100", "Nsample500",
                 "Alpha0", "Alpha-0.5", "Alpha0.5",
                 "Beta1", "Beta0.7", "Beta1.3",
                 "SDy0.1", "SDy0.01", "SDy0.2",
                 "Taux0.1", "Taux0.01", "Taux0.2",     
                 "Tauxy1", "Tauxy0.9", "Tauxy1.1",
                 "CorrME0", "CorrME0.5", "CorrME0.9",
                 "RatioSDme1", "RatioSDme0.8", "RatioSDme1.2", "RatioSDme1.5"),
    scenario_param = c("Nsample", "Nsample", "Nsample",
                       "Alpha", "Alpha", "Alpha",
                       "Beta", "Beta", "Beta",
                       "SDy", "SDy", "SDy",
                       "Taux", "Taux", "Taux", "Taux",
                       "Tauxy", "Tauxy", "Tauxy",
                       "CorrME", "CorrME", "CorrME",
                       "RatioSDme", "RatioSDme", "RatioSDme", "RatioSDme"),
    scenario_value = c("200", "100", "500",
                       "0", "-0.5", "0.5",
                       "1", "0.7", "1.3",
                       "0.1", "0.01", "0.2",
                       "0.1", "0.01", "0.2",     
                       "1", "0.9", "1.1",
                       "0", "0.5", "0.9",
                       "1", "0.8", "1.2", "1.5")
  )
  
  df %>%
    mutate(
      scenario = gsub("mcmcdx_|predeval_|predcompare_", "", scenario)
    ) %>%
    left_join(df_scenarios, by = "scenario")
}

