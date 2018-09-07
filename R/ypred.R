#' Predicted probabilities
#'
#' \code{ypred} calculates the predicted probabilities at the values of the
#' explanatory variable.
#' @param qp output from quickpsy
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' ypred(fit)
#' @export ypred
ypred <- function(averages, param, psych_fun, x, log) {
  one_ypred <- function(averages, param, psych_fun, x, log) {
    x <- averages %>% select(!!x) %>% pull()

    y <- psych_fun$fun[[1]](x, param$par)
    print(tibble(x, y))
    tibble(x, y)
  }

  averages_df <- averages %>% nest(everything(), .key = "averages")
  param_df <- param %>% nest(everything(), .key = "param")
  psych_fun_df <- psych_fun %>% nest(everything(), .key = "psych_fun")

  if (!("dummy_group" %in% names(averages_df)) &
      ("dummy_group") %in% names(psych_fun_df)) {

    df <- averages_df %>%
      left_join(param_df, by = group_vars(param)) %>%
      mutate(psych_fun = psych_fun_df$psych_fun)

  }
  else {
    df <- averages_df %>%
      left_join(param_df, by = group_vars(param)) %>%
      left_join(psych_fun_df, by = group_vars(psych_fun))
  }

  df %>%
    mutate(temp = pmap(list(averages, param, psych_fun), one_ypred, x, log)) %>%
    select(-averages, -param, -psych_fun) %>%
    unnest(temp) %>%
    group_by(UQS(groups(averages)))


 #apply_to_three_elements(averages, param, psych_fun, one_ypred, x, log)

}
