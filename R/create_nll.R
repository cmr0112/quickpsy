#' Creates the negative log-likelihood function
#' \code{create_nll} Creates the negative log-likelihood function
#' @keywords internal
#' @export create_nll
#'
create_nll <- function(averages, psych_fun, x) {
  if (group_vars(psych_fun) == "dummy_group") {
    nlls <- averages %>%
      nest() %>%
      bind_cols(psych_fun)

  }
  else {
    nlls <- averages %>%
      group_by(!!!groups(psych_fun)) %>%
      nest() %>%
      left_join(psych_fun, by = group_vars(psych_fun)) %>%
      unnest(data, .drop = FALSE)

  }

  eps <- .Machine$double.eps

  function(p) {


      #x <- averages %>% select(!!x) %>% pull()
      x <- averages[[quo_name(x)]]

      #phi <- psych_fun(x, p)

      #parameters <- map(x, ~list(., p))
      parameters <- lapply(x, function(y) list(y, p))

      phi <- invoke_map_dbl(nlls$fun, parameters)

      phi[phi < eps] <- eps
      phi[phi > (1 - eps)] <- 1 - eps

      -sum(lchoose(averages$n, averages$k) + # includes the binomial coef
             averages$k * log(phi) + (averages$n - averages$k) * log(1 - phi))

    }

}


