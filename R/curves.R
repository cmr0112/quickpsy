#' Creates the curves
#' \code{curves} creates the curves
#' @import dplyr
#' @keywords internal
#' @export curves
curves <- function(par, limits, log, psyfunguesslapses) {
  one_curve <- function(par, limits, log, psyfunguesslapses) {
    x <- seq(limits$xmin, limits$xmax, length = 300)
    y <- psyfunguesslapses(x, par$par)
    if (log) x <- exp(x)
    tibble(x, y)
  }

  apply_to_two_elements(par, limits,
                        ~one_curve(.x, .y, log, psyfunguesslapses))

}
