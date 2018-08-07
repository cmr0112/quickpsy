#' \code{nll_fun} Creates the negative log-likelihood function
#' @keywords internal
#' @export nll_fun
nll_fun <- function(averages, psych_fun, x) {

  if (group_vars(psych_fun) == "dummy_group" ) {
    groups <- group_vars(averages)
  }
  else {
    groups <- group_vars(averages) %>% setdiff(group_vars(psych_fun))
  }

  averages_df <- averages %>%
    group_by_at(vars(groups)) %>%
    nest(.key = averages)

  averages_df %>%
    mutate(psych_fun = list(psych_fun)) %>%
    mutate(nll_fun = map2(averages, psych_fun, create_nll, x)) %>%
    group_by_at(vars(groups)) %>%
    select(-averages, -psych_fun)

}
