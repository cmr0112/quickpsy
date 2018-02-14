#' Calculates the parameters
#' \code{parameters} calculates the parameters
#' @keywords internal
#' @export
parameters <- function(averages, parini, psyfunguesslapses_df, funname_df,
                       x, guess, lapses) {

  averages_df <- averages %>% nest(everything(), .key = averages)
  parini_df <- parini %>% nest(everything(), .key = parini)

  if (length(groups) != 0)
    df <- averages_df %>%
    left_join(parini_df, by = group_vars(averages)) %>%
    left_join(psyfunguesslapses_df, by = group_vars(averages)) %>%
    left_join(funname_df, by = group_vars(averages))
  else
    df <- averages_df %>%
    bind_cols(parini_df) %>%
    bind_cols(psyfunguesslapses_df) %>%
    bind_cols(funname_df)

  if (!("parmin" %in% names(parini))) {
    df <- df %>%
      rowwise() %>%
      mutate(par = list(par_df(averages, parini, psyfunguesslapses, funname,
                                   x, guess, lapses)))
  }
  else {
    df <- df %>%
    mutate(par = map2(averages, parini,
                         ~lbfgsb(.x, .y, x, psyfunguesslapses)))
  }


  df %>%
    select(-averages) %>%
    unnest(par) %>%
    group_by(UQS(groups(averages)))

}

