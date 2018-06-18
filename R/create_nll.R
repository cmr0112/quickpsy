#' Creates the negative log-likelihood function
#' \code{create_nll} Creates the negative log-likelihood function
#' @keywords internal
#' @export create_nll
create_nll <- function(d, fun_df, x) {

  calculate_nll <- function(d, fun) {
    x <- d %>% select(!!x) %>% pull()
    eps <- .Machine$double.eps

    phi <- fun(x, p)
    phi[phi < eps] <- eps
    phi[phi > (1 - eps)] <- 1 - eps

    -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
  }

  function(p) {
    calculate_nll <- function(d, fun) {
      x <- d %>% select(!!x) %>% pull()
      eps <- .Machine$double.eps

      phi <- fun(x, p)
      phi[phi < eps] <- eps
      phi[phi > (1 - eps)] <- 1 - eps

      -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
    }

    d %>%
      ungroup() %>% # a lo mejor no hace falta
      group_by_at(group_vars(fun_df)) %>%
      nest() %>%
      left_join(fun_df, by = group_vars(fun_df)) %>%
      mutate(nll = map2_dbl(data, fun, calculate_nll)) %>%
      summarise(nll = sum(nll))

  }
}




