calculate_predictions_dif <- function (df, x_seq){
  df %>% 
    mutate(prob = map(dif_slope, augment, 
                      newdata = x_seq, 
                      type.predict = "response")) %>% 
    dplyr::select(-data, -dif_slope) %>% 
    unnest() %>% 
    mutate(duration = 10^log10_duration)
}
