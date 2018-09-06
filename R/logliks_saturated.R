#' Calculates the saturated loglikelihoods
#' \code{logliks} calculates the saturatedloglikelihoods.
#' @export
logliks_saturated <- function(nll_fun_saturated, averages) {

  one_loglik_saturated <- function(nll_fun_saturated, averages_by_nll) {
    p <- c(0) # it doesn't matter as nll_fun_saturated does not depend on it
    nll <- nll_fun_saturated$nll_fun[[1]](p)
    nll %>%
      transmute(loglik = -nll) %>%
      mutate(n_par = length(averages_by_nll$k))
  }

  averages_by_nll <- averages %>%
    group_by(!!!(groups(nll_fun_saturated)))

  apply_to_two_elements(nll_fun_saturated, averages_by_nll, one_loglik_saturated)
}


