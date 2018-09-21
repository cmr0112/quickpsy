#' Perform the averages
#' \code{averages} perform the averages
#' @keywords internal
#' @export averages
averages <- function(d, x, k, n, groups, log) {

  groups_x <- c(groups, quo_name(x))

  if (is.null(n)) {
    d <- d %>% rename(k = !!k)
    averages <- d %>%
      group_by(!!!syms(groups_x)) %>%
      summarise(n = n(), k = sum(k))
  }
  else {
    averages <- d %>%
      group_by(!!!syms(groups_x)) %>%
      transmute(k = !!k, n = !!n)
  }



  if (!is.null(groups)) averages <- averages %>% group_by(!!!groups)

  averages <- averages %>% mutate(prob = k / n)

  if (log) averages <- averages %>% mutate(!!quo_name(x) := log(!!x))

  averages
}


