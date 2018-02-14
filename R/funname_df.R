#' Data frame of functions
#'
#' \code{functions_df} data frame of functions
#' @export funname_df
funname_df <- function(conditions, funname) {
  if (is.character(funname)) {
    funname_df <- conditions %>%
      mutate(funname = list(funname))
  }
  else {
    if (is_tibble(funname)) funname_df <- funname
  }
    funname_df
}
