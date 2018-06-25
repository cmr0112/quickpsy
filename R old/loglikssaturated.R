#' Calculates the loglikelihoods of the saturated model
#'
#' \code{loglikssaturated} calculates the  loglikelihoods of the saturated model.
#' @param qp output from quickpsy
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' loglikssaturated(fit)
#' @export
loglikssaturated <- function(averages, par, x, k, n, psyfunguesslapses) {

  one_logliksaturated <- function(averages, par, x, k, n, psyfunguesslapses) {
    nllfun <- create_nllsaturated(averages, x, k, n, psyfunguesslapses)
    tibble(loglik = -nllfun(par$par))
  }

  apply_to_two_elements(averages, par,
                        ~one_logliksaturated(.x, .y, x, k, n,
                                             psyfunguesslapses))

}


