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

  one_avbootstrap <- function(averages, ypred, bootstrap, B) {

    averages_for_boot <- averages

    if (bootstrap == "parametric") averages_for_boot$prob <- ypred$y

    tibble(sample = 1:B) %>%
      crossing(averages_for_boot) %>%
      group_by(sample) %>%
      mutate(k = rbinom(n(), size = n, prob = prob)) %>%
      dplyr::select(-prob)
  }

  apply_to_two_elements(averages, ypred, one_avbootstrap, bootstrap, B)
}


