#' Apply function to three elements of a list
#' @export apply_to_three_elements
apply_to_three_elements <- function(x, y, z, q, ...) {


  enq_x <- enquo(x)
  x_df <- x %>% nest(everything(), .key = !!enq_x)

  enq_y <- enquo(y)
  y_df <- y %>% nest(everything(), .key = !!enq_y)

  enq_z <- enquo(z)
  z_df <- z %>% nest(everything(), .key = !!enq_z)


  extra_vars <- quos(...)

  print(z)
  print(z_df)
  print(group_vars(z))

  if (group_vars(y) == "dummy_group") {
    x_df_g <- x_df %>%
      mutate(dummy_group = "g")

    df <- x_df_g %>%
      left_join(y_df, by = group_vars(y))
  }
  else {
    df <- x_df %>%
      left_join(y_df, by = group_vars(y))
  }

  if (group_vars(z) == "dummy_group") {
    df_g <- df %>%
      mutate(dummy_group = "g")

    df <- df_g %>%
      left_join(z_df, by = group_vars(z))
  }
  else {
    df <- df %>%
      left_join(y_df, by = group_vars(y))
  }


  # if (length(groups(z)) != 0) {
  #   df <- df %>%
  #     left_join(z_df, by = group_vars(z))
  # }
  # else {
  #   df <- df %>%
  #     mutate(!!quo_name(enq_z) := list(z_df))
  # }
  #


  # if (length(groups(x)) != 0) {
  #   df <- x_df %>%
  #   left_join(y_df, by = group_vars(y)) %>%
  #   left_join(z_df, by = group_vars(z))
  # }
  # else {
  #   df <- x_df %>%
  #     bind_cols(y_df) %>%
  #     bind_cols(z_df)
  # }

  df %>%
    mutate(temp = pmap(list(!!enq_x, !!enq_y, !!enq_z), q, !!!extra_vars)) %>%
    dplyr::select(-!!enq_x, -!!enq_y,-!!enq_z) %>%
    unnest(temp) %>%
    group_by(UQS(groups(x)))
}
