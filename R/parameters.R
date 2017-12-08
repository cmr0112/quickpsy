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
  parini_df <- parini %>% nest(everything(), .key = parini)

  if (length(groups) != 0)
    df <- averages_df %>%
    left_join(parini_df, by = groups)
  else
    df <- averages_df %>%
    bind_cols(parini_df)


  if (!("parmin" %in% names(parini)))
    df <- df %>%
    mutate(par = map2(averages, parini,
                      ~par_tibble(.x, .y, x,psyfunguesslapses, funname,
                                  guess, lapses)))

  else
    df <- df %>%
    mutate(par = map2(averages, parini,
                         ~lbfgsb(.x, .y, x, psyfunguesslapses)))


  df %>%
    select(-averages) %>%
    unnest(par) %>%
    group_by(!!!syms(groups))

}

