#' Calculates the loglikelihoods
#' \code{logliks} calculates the loglikelihoods.
#' @param qp output from quickpsy
#' @export
logliks <- function(qp) {

  one_loglik <- function(averages, par, x, psyfunguesslapses) {
    nllfun <- create_full_nll(averages, x, psyfunguesslapses)
    tibble(loglik = -nllfun(par$par))
  }

  apply_to_two_elements(qp,
                        averages, par,
                        ~one_loglik(.x, .y,
                                       qp$x,
                                       qp$psyfunguesslapses))
}


