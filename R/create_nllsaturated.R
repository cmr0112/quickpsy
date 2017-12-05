#' Creates the full saturated negative log-likelihood function
#' \code{create_nll} Creates the full saturated negative log-likelihood function
#' @keywords internal
#' @export
create_nllsaturated <- function(d, x, k, n, psyfunguesslapses){
  function(p) {

    phi <- d$k / d$n
    eps <- .Machine$double.eps
    phi[phi < eps] <- eps
    phi[phi > (1 - eps)] <- 1 - eps
    f <- tibble(k = d$k, n = d$n) %>% mutate(coef = lchoose(n,k))

    -sum(f$coef + d$k * log(phi) + (d$n - d$k * log(1 - phi)))
  }
}


