# predictions-doublesampling.R

#' predict_eval_doublesampling
#' 
#' @param calibration TF calibration data
#' @param validation TF validation data
predict_eval_doublesampling <- function(calibration, validation) {
  
  # fit TF in each calibration dataset
  data_cal <- calibration %>%
    select(tar_batch, tar_rep, scenario_label, 
           nonref_protocol2, ref_protocol1) %>%
    nest(.by = c(tar_batch, tar_rep, scenario_label), .key = "data") %>%
    mutate(
      mod = map(data, function(x) {
        lm(ref_protocol1 ~ 1 + nonref_protocol2, data = x)
      })
    )
  
  # apply fitted TF to make predictions in validation dataset
  data_val <- validation %>%
    select(tar_batch, tar_rep, scenario_label, 
           nonref_protocol2, ref_protocol1) %>%
    nest(.by = c(tar_batch, tar_rep, scenario_label), .key = "data") %>%
    left_join(
      data_cal %>% 
        select(tar_batch, tar_rep, scenario_label, mod),
      by = c("tar_batch", "tar_rep", "scenario_label")
    ) %>%
    mutate(
      pred = map2(data, mod, function(newdat, model) {
        predict(model, newdata = newdat, interval = "prediction") %>%
          as.data.frame()
      })
    ) 
  
  # summarize predictive performance: PICP, R², MAE, MPE, RMSE, SDPE
  data_val %>%
    mutate(
      PICP = map2_dbl(data, pred, function(d, p) {
        mean(between(d$ref_protocol1, p$lwr, p$upr))
      }),
      R2 = map2_dbl(data, pred, function(d, p) {
        1 - sum((d$ref_protocol1 - p$fit)^2) / 
          sum((d$ref_protocol1 - mean(d$ref_protocol1))^2) 
      }),
      MAE = map2_dbl(data, pred, function(d, p) {
        mean( abs(d$ref_protocol1 - p$fit) )
      }),
      MPE = map2_dbl(data, pred, function(d, p) {
        mean( d$ref_protocol1 - p$fit )
      }),
      RMSE = map2_dbl(data, pred, function(d, p) {
        sqrt( mean( (d$ref_protocol1 - p$fit)^2 ) )
      }),
      SDPE = map2_dbl(data, pred, function(d, p) {
        sd((d$ref_protocol1 - p$fit))
      })
    ) %>%
    select(tar_batch, tar_rep, scenario_label,
           PICP, R2, MAE, MPE, RMSE, SDPE)
}
