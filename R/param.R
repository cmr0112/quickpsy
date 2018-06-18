#' \code{par} Param
#' @keywords internal
#' @export param
param <- function(nll_fun, parini) {
  calculate_par <- function(parini, nll_fun) {
    par <- optim(parini$par, nll_fun)$p
    tibble(parn = paste0('p', seq(1, length(par))), par = par)
  }

  parini %>%
    group_by_at(vars(group_vars(nll_fun))) %>%
    nest(.key = "parini") %>%
    left_join(nll_fun, by = group_vars(nll_fun)) %>%
    mutate(par = map2(parini, nll_fun, calculate_par)) %>%
    select(-nll_fun, -parini) %>%
    unnest(par)
}
