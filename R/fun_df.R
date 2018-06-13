#' Data frame of functions
#'
#' \code{fun_df} data frame of functions
#' @export fun_df
fun_df <- function(conditions, fun) {
  if (is.function(fun)) {
    fun_df <- conditions %>% mutate(fun = list(fun))
  }
  else {
    if (is_tibble(fun)) fun_df <- fun
  }

    fun_df
}
