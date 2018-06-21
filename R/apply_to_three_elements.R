#' Apply function to two elements of a list
#' @export apply_to_three_elements
apply_to_three_elements <- function(x, y, z, q, ...) {

  enq_x <- enquo(x)
  x_df <- x %>% nest(everything(), .key = !!enq_x)

  enq_y <- enquo(y)
  y_df <- y %>% nest(everything(), .key = !!enq_y)
  enq_z <- enquo(z)

  z_df <- z %>% nest(everything(), .key = !!enq_z)

  extra_vars <- quos(...)

  if (length(groups(x)) != 0) {
    df <- x_df %>%
    left_join(y_df, by = group_vars(y)) %>%
    left_join(z_df, by = group_vars(z))
  }
  else {
    df <- x_df %>%
      bind_cols(y_df) %>%
      bind_cols(z_df)

  }

  df %>%
    mutate(temp = pmap(list(!!enq_x, !!enq_y, !!enq_z), q, !!!extra_vars)) %>%
    select(-!!enq_x, -!!enq_y,-!!enq_z) %>%
    unnest(temp) %>%
    group_by(UQS(groups(x)))
}
