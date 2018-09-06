#' Calculates the saturated loglikelihoods
#' \code{logliks} calculates the saturated loglikelihoods.
#' @export
loglikssaturated <- function(nll_fun, param) {

  one_loglik <- function(nll_fun, param) {
    nll <- nll_fun$nll_fun[[1]](param$par)
    nll %>% transmute(loglik = -nll)
  }

  apply_to_two_elements(nll_fun, param, one_loglik)
}


