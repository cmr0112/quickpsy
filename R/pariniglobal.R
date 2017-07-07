#' Calculates the initial parameters
#' @keywords internal
#' @export
parini <- function(d, x, k, n, guess, lapses, funname, groups) {
  d %>% do(one_parini(., x, k, n, guess, lapses,funname, groups))
}
