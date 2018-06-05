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
ypred <- function(par, averages, x, psyfunguesslapses_df) {
  one_ypred <- function(par, averages, x, psyfunguesslapses) {
    x <- averages %>% select(!!x) %>% pull()
    p <- par %>% select(par) %>% pull()
    y <- psyfunguesslapses(x, p)
    tibble(x, y)
  }

  apply_to_two_elements(par, averages,
                        ~one_ypred(.x, .y, x, psyfunguesslapses))
}
