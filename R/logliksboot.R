#' Calculates the bootstrap loglikelihoods
#'
#' \code{logliksboot} calculates the bootstraploglikelihoods.
#' @param qp output from quickpsy
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' logliksboot(fit)
#' @export
logliksboot <- function(qp) {
  groups <- qp$groups
  groups_sample <- c(groups, "sample")
  if (length(groups) == 0)
    avbootstrap <- qp$avbootstrap %>% group_by(sample)
  else
    avbootstrap <- qp$avbootstrap %>%
      group_by(!!!syms(groups_sample))

  avbootstrap %>%
    do(one_loglik(., qp$x, qp$psyfunguesslapses, groups_sample,
                  qp$parbootstrap))
}




