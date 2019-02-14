#' \code{psy_fun} psych_fun
#' @keywords internal
#' @export psych_fun
psych_fun <- function(fun, guess, lapses) {

  if (is.function(fun)) {
    groups <- syms("dummy_group")
    psych_fun <- create_psy_fun(fun, guess, lapses)
    tibble(fun = list(psych_fun)) %>%
      mutate(dummy_group = "g") %>%
      group_by(!!!groups)

  }
  else {
    groups <- fun %>% dplyr::select(-fun) %>% names() %>% syms()
    #fun %>% group_by_at(vars(groups))
    fun %>% group_by(!!!groups)
  }

}
