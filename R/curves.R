#' Creates the curves
#' \code{curves} creates the curves
#' @import dplyr
#' @keywords internal
#' @export
curves <- function(qp) {
  one_curve <- function(par, limits, log, psyfunguesslapses) {
    x <- seq(limits$xmin, limits$xmax, length = 300)
    y <- psyfunguesslapses(x, par$par)
    if (log) x <- exp(x)
    tibble(x, y)
  }

  apply_to_two_elements(qp,
                        par, limits,
                        ~one_curve(.x, .y, qp$log, qp$psyfunguesslapses))

}
