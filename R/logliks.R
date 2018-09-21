#' Calculates the loglikelihoods
#' \code{logliks} calculates the loglikelihoods.
#' @param qp output from quickpsy
#' @export
logliks <- function(nll_fun, param) {

  one_loglik <- function(nll_fun, param) {
    nll <- nll_fun$nll_fun[[1]](param$par)

    nll %>%
      transmute(loglik = -nll) %>%
      mutate(n_par = length(param$par))
  }

  apply_to_two_elements(nll_fun, param, one_loglik)
}


