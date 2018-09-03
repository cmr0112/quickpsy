#' \code{param} Param
#' @keywords internal
#' @export param
param <- function(nll_fun, parini) {
  calculate_par <- function(parini, nll_fun) {

    if ("par" %in% names(parini)) {
      param <- optim(parini$par,
                     nll_fun$nll_fun[[1]])$par


    }
    else {
      param <- optim(.5 * (parini$parmax - parini$parmin),
                     nll_fun$nll_fun[[1]],
                     method = "L-BFGS-B",
                     lower = parini$parmin,
                     upper = parini$parmax)$par
    }


    tibble(parn = paste0("p", seq(1, length(param))), par = param)
  }

  apply_to_two_elements(parini, nll_fun, calculate_par)
}
