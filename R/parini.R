#' \code{parini} parini
#' @keywords internal
#' @export parini
parini <- function(averages, parini, psych_fun) {

  if (group_vars(psych_fun) == "dummy_group") groups <- groups(averages)
  else groups <- groups(averages) %>% setdiff(groups(psych_fun))

  conditions <- averages %>%
    ungroup() %>%
    distinct(!!!groups)


  if (is.atomic(parini)) {
    parini <- conditions %>%
      crossing(tibble(parn = paste0("p", seq(1, length(parini))),
                      par = parini))
  }

  else if (is.list(parini) & !is.data.frame(parini)) {
    parini <- matrix(unlist(parini), ncol = 2, byrow = TRUE)

    parini <- conditions %>%
      crossing(tibble(parn = paste0("p", seq(1, length(parini[,1]))),
                      parmin = parini[,1],
                      parmax = parini[,2]))
  }

  else if (is.data.frame(parini)) {
    parini <- conditions %>%
      crossing(parini)

  }

  print(parini)

  parini %>%
    group_by(!!!groups)
}
