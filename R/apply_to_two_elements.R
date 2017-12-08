#' Apply function to two elements of a list
#' @export apply_to_two_elements
apply_to_two_elements <- function(x, y, q) {

  enq_x <- enquo(x)

  x_df <- x %>% nest(everything(), .key = !!enq_x)

  enq_y <- enquo(y)
  y_df <- y %>% nest(everything(), .key = !!enq_y)

  if (length(groups(x)) != 0)
    df <- x_df %>% left_join(y_df, by = group_vars(x))
  else df <- x_df %>% bind_cols(y_df)

  df %>%
    mutate(temp = map2(!!enq_x, !!enq_y, q)) %>%
    select(-!!enq_x, -!!enq_y) %>%
    unnest(temp) %>%
    group_by_at(group_vars(x))




  # x <- enquo(x)
  # x_df <- l %>% pluck(quo_name(x)) %>%
  #   group_by(!!! syms(l$groups)) %>%
  #   nest(everything(), .key = !!x)
  #
  # y <- enquo(y)
  # y_df <- l %>% pluck(quo_name(y)) %>%
  #   group_by(!!! syms(l$groups)) %>%
  #   nest(everything(), .key = !!y)
  #
  # if (length(l$groups) != 0)
  #   df <- x_df %>% left_join(y_df, by = l$groups)
  # else df <- x_df %>% bind_cols(y_df)
  #
  # df %>%
  #   mutate(temp = map2(!!x, !!y, q)) %>%
  #   select(-!!x, -!!y) %>%
  #   unnest(temp)

}
