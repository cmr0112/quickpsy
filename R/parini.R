#' Calculates the initial parameters
#' @keywords internal
#' @export parini
parini <- function(d, x, guess, lapses, funname) {
  d %>% do(one_parini(., x, guess, lapses, funname))
}
