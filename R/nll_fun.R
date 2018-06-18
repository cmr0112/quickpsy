#' \code{nll_fun} Creates the negative log-likelihood function
#' @keywords internal
#' @export nll_fun
nll_fun <- function(averages, fun, x) {

  groups <- group_vars(averages) %>% setdiff(group_vars(fun))

  averages_df <- averages %>%
    group_by_at(vars(groups)) %>%
    nest(.key = averages)

  averages_df %>%
    mutate(fun = list(fun_df)) %>%
    mutate(nll_fun = map2(averages, fun, ~create_nll(.x, .y, x))) %>%
    group_by_at(vars(groups)) %>%
    select(-averages, -fun)
}
