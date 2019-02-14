#' Calculates the Akaike Information Criterion
#' \code{akaike} calculates the loglikelihoods.
#' @export
akaike <- function(logliks, param) {

  one_akaike <- function(logliks, param) {
    tibble(loglik = logliks$loglik,
           n_par = length(param$par),
           aic = -2 * logliks$loglik + 2 * n_par)
  }

  apply_to_two_elements(logliks, param, one_akaike)
}


