#' Calculates the deviances
#'
#' \code{deviance} calculates the deviances.
#' @param qp output from quickpsy
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' deviance(fit)
#' @export
deviance <- function(logliks, loglikssaturated) {
  one_deviance <- function(logliks, loglikssaturated) {
    deviance <- -2 * (logliks$loglik - loglikssaturated$loglik)
    tibble(deviance)
  }

  apply_to_two_elements(logliks, loglikssaturated, ~one_deviance(.x, .y))
}


