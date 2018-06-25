#' Creates bootstrap samples of the parameters
#'
#' \code{parbootstrap} creates bootstrap samples of the parameters.
#' @param qp output from quickpsy
#' @export
parbootstrap <- function(qp) {
  # if (qp$pariniset) {
  #   if (is.atomic(parini)) {
  #     parini <- qp$par
  #     pariniset <- FALSE
  #   }
  #   else{
  #     parini <- qp$parini
  #     pariniset <- TRUE
  #   }
  # }
  # else {
  #   parini <- qp$par
  #   pariniset <- FALSE
  # }

  # if (length(qp$groups) == 0)
  #   avboot <- qp$avbootstrap %>% group_by_('sample')
  # else
    avboot <- qp$avbootstrap %>%
      group_by(!!!syms(qp$groups))
  #
  # avboot %>%
  #   do(one_parameters(., qp$x, qp$k, qp$n, qp$psyfunguesslapses, qp$funname,
  #                     parini, pariniset, qp$guess, qp$lapses,
  #                     qp$optimization, qp$groups))
#hay un problema que en parameters los dfs tienen que estar agrupados
    print(avboot)
  avboot %>%
    group_by(sample) %>%
    nest(.key = averages) %>%
    mutate(parini = list(qp$parini)) %>%
    mutate(temp = map2(averages, parini,
                       ~parameters(.x, .y,
                                   qp$x, qp$k, qp$n,
                                   qp$psyfunguesslapses, qp$funname,
                                   qp$pariniset, qp$guess, qp$lapses,
                                   qp$groups)))


}


