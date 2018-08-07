#' Creates the negative log-likelihood function
#' \code{create_nll} Creates the negative log-likelihood function
#' @keywords internal
#' @export create_nll
#'
create_nll <- function(averages, psych_fun, x) {
  function(p) {
    calculate_nll <- function(averages, psych_fun) {
      x <- averages %>% select(!!x) %>% pull()
      eps <- .Machine$double.eps

      phi <- psych_fun(x, p)
      phi[phi < eps] <- eps
      phi[phi > (1 - eps)] <- 1 - eps

      -sum(averages$k * log(phi) + (averages$n - averages$k) * log(1 - phi))
    }

    if (group_vars(psych_fun) == "dummy_group") {
      nlls <- averages %>%
        nest() %>%
        bind_cols(psych_fun)
       #mutate(fun = psych_fun$psych_fun)

    }
    else {
      nlls <- averages %>%
        group_by(!!!groups(psych_fun)) %>%
        nest() %>%
        left_join(psych_fun, by = group_vars(psych_fun))
    }

    nlls %>%
      mutate(nll = map2_dbl(data, fun, calculate_nll)) %>%
      summarise(nll = sum(nll))

  }
}


