#' Apply function to data frames
#' @export apply_fun

#
# one_fun <- function(averages, limits) {
#   averages
# }
#
# apply_fun <- function(l, q, ...) {
#   quo_l <- enquo(l)
#
#   df <- imap(l, function(df, name) nest(df, .key = !!name)) %>%
#     reduce(left_join)
#
#   df %>% mutate(temp = pmap(!!!quo_l, one_fun)) #%>%
#     # unnest(temp) #%>%
#   #   select(-!!names(l))
#
# }
#
#
# x <- apply_fun(list(averages = averages,
#                     limits = limits),
#                ~one_fun)
#
# one_fun <- function(averages, limits) {
#   averages
# }
# x %>% mutate(temp = pmap(list(averages, limits), one_fun))
#
# one_fun <- function(limits, averages) {
#   limits
# }
#
# l <- list(averages, limits)
# xx <- x %>%
#   mutate(kk = pmap(list(averages, limits), one_fun))

