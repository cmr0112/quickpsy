#' Calculates the parameters
#' \code{parameters} calculates the parameters
#' @keywords internal
#' @export
parameters <- function(averages, parini,
                       x, k, n,
                       psyfunguesslapses, funname,
                       pariniset, guess, lapses,
                       groups) {

  averages_df <- averages %>% nest(everything(), .key = averages)

  if (is.tibble(parini) | is.atomic(parini))  {
    if (is.tibble(parini)) {
      parini_df <- parini %>%
        nest(everything(), .key = parini)
      if (length(groups) != 0)
        df <- averages_df %>%
          left_join(parini_df, by = groups)
      else df <- averages_df %>%
          bind_cols(parini_df)
    }

    if (is.atomic(parini)) {
      parini <- tibble(paran = paste0('p', seq(1, length(parini))),
                       par = parini)
      df <- averages_df %>% add_column(parini = list(parini))
    }

    df <- df %>%
      mutate(par = map2(averages, parini,
                        ~par_tibble(.x, .y,
                                     x, k, n,
                                     psyfunguesslapses, funname,
                                     pariniset,
                                     guess, lapses)))
  }
  else {
    if (is.list(parini)) {
      df <- averages_df %>%
        mutate(par = map(averages,
                          ~lbfgsb(.x, parini,
                                       x, k, n,
                                       psyfunguesslapses, funname,
                                       pariniset,
                                       guess, lapses)))
    }
  }

  df %>%
    select(-averages) %>%
    unnest(par) %>%
    group_by(!!!syms(groups))


}

