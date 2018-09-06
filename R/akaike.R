#' Calculates the Akaike Information Criterion
#' \code{akaike} calculates the loglikelihoods.
#' @export
akaike <- function(logliks, param) {

  one_akaike <- function(logliks, param) {
    tibble(k = length(param$par), aic = -2 * logliks$loglik + 2 * k)
  }

  apply_to_two_elements(logliks, param, one_akaike)
}


