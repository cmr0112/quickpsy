#' \code{param} Param
#' @keywords internal
#' @export param
param <- function(nll_fun, parini) {
  calculate_par <- function(parini, nll_fun) {


    if ("par" %in% names(parini)) {
      param <- optim(parini$par,
                     nll_fun$nll_fun[[1]])$par



    }
    else {
      param <- optim(.5 * (parini$parmax - parini$parmin),
                     nll_fun$nll_fun[[1]],
                     method = "L-BFGS-B",
                     lower = parini$parmin,
                     upper = parini$parmax)$par
    }

    tibble(parn = paste0("p", seq(1, length(param))), par = param)
  }

  apply_to_two_elements(parini, nll_fun, calculate_par)

 # parini_df <- parini %>% nest(everything(), .key = "parini")
 #
 # nll_fun_df <- nll_fun %>% nest(everything(), .key = "nll_fun")
 #
 #
 # df <- parini_df %>%
 #   left_join(nll_fun_df, by = group_vars(parini))

 #print(df)
 #print(df$nll_fun[[1]]$nll_fun[[1]])


 # xx <- Vernier %>%
  #  filter(WaveForm == "Sine", TempFreq == 2, Direction == "Upward")

  #print(nll_fun_df$nll_fun[[1]]$nll_fun[[1]])

  #for (i in 1:8) optim(c(1,1), nll_fun_df$nll_fun[[1]]$nll_fun[[1]])


  # df %>%
  #   mutate(temp = map2(parini, nll_fun, calculate_par)) %>%
  #   select(-parini, -nll_fun) %>%
  #   unnest(temp) %>%
  #   group_by(UQS(groups(parini)))

  #tibble(x = 5)
}
