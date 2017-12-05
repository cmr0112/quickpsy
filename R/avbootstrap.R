#' Creates bootstrap samples
#'
#' \code{avbootstrap} creates bootstrap samples
#' @param qp output from quickpsy
#' @param bootstrap \code{'parametric'} performs parametric bootstrap;
#' \code{'nonparametric'} performs non-parametric bootstrap;
#' \code{'none'} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is 100 ONLY).
#' @export
avbootstrap <- function(qp, bootstrap, B) {

  one_bootstrapav <- function(averages, ypred,
                              x, k, n,
                              bootstrap, B) {

    if (bootstrap == "parametric") ypred <- ypred$y
    if (bootstrap == "nonparametric") ypred <- averages$k / averages$n

    create_fake_data <- function(averages, ypred){
      x <- averages %>% select(!!x) %>% pull()
      kfake <- rbinom(length(x), averages$n, ypred)
      averages %>% mutate(k = kfake, prob = k / n)
    }

    tibble(sample = 1:B) %>%
      group_by(sample) %>%
      mutate(temp = list(create_fake_data(averages, ypred))) %>%
      unnest(temp)

  }

  apply_to_two_elements(qp,
                        averages, ypred,
                        ~one_bootstrapav(.x, .y,
                                         qp$x, qp$k, qp$n,
                                         bootstrap, B))

}
