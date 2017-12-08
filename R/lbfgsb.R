#' L-BFGS-B optimization
#' @keywords internal
#' @export
lbfgsb <- function(d, parini, x, psyfunguesslapses) {
  nllfun <- create_nll(d, x, psyfunguesslapses)

  para <- optim(.5 * (parini$parmin + parini$parmax),
                nllfun,
                method = "L-BFGS-B",
                lower = parini$parmin,
                upper = parini$parmax)$par

  tibble(parn = paste0("p", seq(1, length(para))), par = para)
}
