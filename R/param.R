#' \code{par} Param
#' @keywords internal
#' @export param
param <- function(nll_fun, parini) {
  calculate_par <- function(parini, nll_fun) {
    param <- optim(parini$par, nll_fun$nll_fun[[1]])$par
    tibble(parn = paste0('p', seq(1, length(param))), par = param)
  }

  apply_to_two_elements(parini, nll_fun, calculate_par)
}
