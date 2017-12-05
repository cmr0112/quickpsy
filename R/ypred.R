#' Predicted probabilities
#'
#' \code{ypred} calculates the predicted probabilities at the values of the
#' explanatory variable.
#' @param qp output from quickpsy
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' ypred(fit)
#' @export ypred
ypred <- function(qp) {
  one_ypred <- function(par, averages, x, psyfunguesslapses) {
    x <- averages %>% select(!!x) %>% pull()
    y <- psyfunguesslapses(x, par %>% select(par) %>% pull())
    tibble(x, y)
  }

  apply_to_two_elements(qp,
                        par, averages,
                        ~one_ypred(.x, .y, qp$x, qp$psyfunguesslapses))
}
