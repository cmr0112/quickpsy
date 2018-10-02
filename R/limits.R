#' Creates the limits
#' \code{limits} creates the limits
#' @keywords internal
#' @export
limits <- function(averages, x, xmin, xmax) {
  one_limit <- function(averages, x, xmin, xmax) {
    #if (is.null(xmin)) xmin <- averages %>% select(!!x) %>% pull() %>% min()
    #if (is.null(xmax)) xmax <- averages %>% select(!!x) %>% pull() %>% max()
    if (is.null(xmin)) xmin <- averages[[quo_name(x)]] %>% min()
    if (is.null(xmax)) xmax <- averages[[quo_name(x)]] %>% max()

    tibble(xmin, xmax)
  }

  averages #%>%
    # nest(everything(), .key = averages) %>%
    # mutate(limits = map(averages, one_limit, x, xmin, xmax)) %>%
    # select(-averages) %>%
    # unnest(limits) %>%
    # group_by(!!!(groups(averages)))


}

