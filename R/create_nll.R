#' Creates the negative log-likelihood function
#' \code{create_nll} Creates the negative log-likelihood function
#' @keywords internal
#' @export create_nll
create_nll <- function(d, x, psyfunguesslapses){
  function(p) {
    x <- d %>% select(!!x) %>% pull()
    eps <- .Machine$double.eps

    phi <- psyfunguesslapses(x, p)
    phi[phi < eps] <- eps
    phi[phi > (1 - eps)] <- 1 - eps

    -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
  }
}


