#'
#' @export model_selection_lrt
model_selection_lrt <- function(qp_loglik1, qp_loglik2, alpha = .05){

  qp_loglik1 <- qp_loglik1 %>% rename(loglik1 = loglik, n_par1 = n_par)
  qp_loglik2 <- qp_loglik2 %>% rename(loglik2 = loglik, n_par2 = n_par)


  if (is.null(groups(qp_loglik1))) {
    qp_loglik <- qp_loglik1 %>%
      bind_cols(qp_loglik2)
  }
  else {
    qp_loglik <- qp_loglik1 %>%
      left_join(qp_loglik2, by = group_vars(qp_loglik1))
  }

  qp_loglik %>%
    mutate(target = if_else(n_par1 > n_par2, "first", "second"),
           null = if_else(n_par1 > n_par2, "second", "first"),
           loglik_target = if_else(target == "first", loglik1, loglik2),
           loglik_null = if_else(n_par1 > n_par2, loglik2, loglik1),
           deviance = -2 * (loglik_null - loglik_target),
           p.value =  pchisq(deviance, abs(n_par1 - n_par2),
                             lower.tail = FALSE),
           best = if_else( (p.value < alpha), target, null)) %>%
    select(-target, -null, -loglik_target, -loglik_null)

}

