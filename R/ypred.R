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
ypred <- function(averages, param, psy_fun, x) {
  one_ypred <- function(averages, param, psy_fun, x) {
    x <- averages %>% select(!!x) %>% pull()
    y <- psy_fun$fun[[1]](x, param$par)
    tibble(x, y)
  }

 apply_to_three_elements(averages, param, psy_fun, one_ypred, x)

}
