#' Calculates the confidence intervals for the thresholds
#' \code{thresholdsci} calculates the confidence intervals for the thresholds
#' @keywords internal
#' @export thresholdsci
thresholdsci <- function(thresholds, thresholdsbootstrap, ci) {
  ci <- thresholdsbootstrap %>%
    group_by(!!!(groups(thresholds))) %>%
    summarise(threinf =  quantile(thre, .5*(1 - .95)),
              thresup = quantile(thre, 1 - .5*(1 - .95)))

  thresholds %>% left_join(ci, by = group_vars(thresholds))
}

