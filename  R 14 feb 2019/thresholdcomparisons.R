#' Pair comparisons of the thresholds using bootstrap
#' \code{thresholdscomparisons} Calculates the bootstrap confidence intervals for the
#' difference in the thresholds for two groups for all possible pairs
#' of groups
#' @keywords internal
#' @export thresholdcomparisons
thresholdcomparisons <- function(thresholds_dif, thresholds_difbootstrap, ci) {
  thresholds_difbootstrap %>%
    group_by_at(vars(setdiff(names(thresholds_dif),
                             c("thre", "thre2", "dif")))) %>%
    summarise(difinf = quantile(dif, .5*(1 - ci))[[1]],
              difsup = quantile(dif, 1 - .5*(1 - ci))[[1]],
              signif = ifelse(difinf * difsup < 0, "", "*"))
}
