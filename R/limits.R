#' Creates the limits
#' \code{limits} creates the limits
#' @keywords internal
#' @export
limits <- function(averages, x, xmin, xmax) {
  one_limit <- function(averages, x, xmin, xmax) {
    if (is.null(xmin)) xmin <- averages %>% select(!!x) %>% pull() %>% min()
    if (is.null(xmax)) xmax <- averages %>% select(!!x) %>% pull() %>% max()
    tibble(xmin, xmax)
  }
  averages %>%
    nest(everything(), .key = averages) %>%
    mutate(limits = map(averages, ~one_limit(.x, x, xmin, xmax))) %>%
    select(-averages) %>%
    unnest(limits) #%>%
   # group_by(UQS(groups(averages)))
}

