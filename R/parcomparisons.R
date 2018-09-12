#' Pair comparisons of the parameters using bootstrap
#' \code{parcomparisons} Calculates the bootstrap confidence intervals for the
#' difference in the parameters for two groups for all possible pairs
#' of groups
#' @keywords internal
#' @export parcomparisons
parcomparisons <- function(par_dif, par_difbootstrap, ci) {
  par_difbootstrap%>%
    group_by_at(vars(setdiff(names(par_dif),
                             c("par", "par2", "dif")))) %>%
    summarise(difinf = quantile(dif, .5*(1 - ci))[[1]],
              difsup = quantile(dif, 1 - .5*(1 - ci))[[1]],
              signif = ifelse(difinf * difsup < 0, "", "*"))
}
