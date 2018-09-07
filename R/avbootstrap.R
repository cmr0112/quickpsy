#' Creates bootstrap samples
#'
#' \code{avbootstrap} creates bootstrap samples
#' @param qp output from quickpsy
#' @param bootstrap \code{'parametric'} performs parametric bootstrap;
#' \code{"nonparametric"} performs non-parametric bootstrap;
#' \code{"none"} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is 100 ONLY).
#' @export
#'
avbootstrap <- function(averages, ypred, bootstrap, B) {

  one_avbootstrap <- function(averages, ypred,bootstrap, B) {
    print(averages)
    print(ypred)
  }

  apply_to_two_elements(averages, ypred, one_avbootstrap,
                        bootstrap, B)
}


