#' Calculates the confidence intervals for the parameters
#' \code{parci} calculates the confidence intervals for the parameters
#' @keywords internal
#' @export parci
parci <- function(par, parbootstrap, ci) {
  ci <- parbootstrap %>%
    group_by(!!!(groups(par))) %>%
    group_by(parn, add = TRUE) %>%
    summarise(parinf =  quantile(par, .5*(1 - .95)),
              parsup = quantile(par, 1 - .5*(1 - .95)))

  par %>% left_join(ci, by = c(group_vars(par), "parn"))


}

