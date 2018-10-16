#'
#' @export model_selection_aic
model_selection_aic <- function(qp_aic1, qp_aic2){

  qp_aic1 <- qp_aic1 %>% rename(loglik1 = loglik, n_par1 = n_par, aic1 = aic)
  qp_aic2 <- qp_aic2 %>% rename(loglik2 = loglik, n_par2 = n_par, aic2 = aic)

  qp_aic1 %>%
    left_join(qp_aic2, by = group_vars(qp_aic1)) %>%
    mutate(best = if_else(aic1 < aic2, "first", "second"),
           p = exp(-(aic1 - aic2)))

}
