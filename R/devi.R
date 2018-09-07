#' Calculates the deviance
#'
#' \code{devi} calculates the deviance
#' @export
devi <- function(logliks, loglikssaturated) {
  one_devi <- function(logliks, loglikssaturated) {
    tibble(deviance = -2 * (logliks$loglik - loglikssaturated$loglik),
           df = loglikssaturated$n_par - logliks$n_par,
           p_value_chi_sqr = pchisq(deviance, df, lower.tail = FALSE))
  }

  apply_to_two_elements(logliks, loglikssaturated, one_devi)

}
