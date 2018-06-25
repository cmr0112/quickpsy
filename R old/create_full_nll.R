#' Creates the full negative log-likelihood function
#' \code{create_full_nll} Creates the full negative log-likelihood function
#' @keywords internal
#' @export
create_full_nll <- function(d, x, psyfunguesslapses) {
  function(p) {

    x <- d %>% select(!!x) %>% pull()
    eps <- .Machine$double.eps
    phi <- psyfunguesslapses(x, p)
    phi[phi < eps] <- eps
    phi[phi > (1 - eps)] <- 1 - eps
    f <- tibble(k = d$k, n = d$n) %>% mutate(coef = lchoose(n, k))

     -sum(f$coef + d$k * log(phi) + (d$n - d$k * log(1 - phi)))

  }
}


