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
      rename(k = !!k, n = !!n)
  }

  if (!is.null(groups)) averages <- averages %>% group_by(!!!syms(groups))

  averages <- averages %>% mutate(prob = k / n)

  name_x <- quo_name(x)
  if (log) averages <- averages %>% mutate(!!name_x := log(!!x))

  averages
}


