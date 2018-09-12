#' @keywords internal
#' @export param_dif
param_dif <- function(param) {


  combinations <- as_tibble(t(combn(nrow(param %>% filter(parn == "p1")), 2)))

  create_df <- function(df) {
    select_rows <- function(V1, V2) {
      cond1 <- df[V1,]
      cond2 <- df[V2,]
      names(cond2) <- paste0(names(cond2), "2")
      bind_cols(cond1, cond2)
    }

    combinations %>%
      rowwise() %>%
      mutate(temp = list(select_rows(V1, V2))) %>%
      unnest(temp) %>%
      mutate(dif = par - par2) %>%
      select(-V1, -V2)
  }

  param %>%
    group_by(parn) %>%
    nest() %>%
    mutate(temp = map(data, create_df)) %>%
    unnest(temp)

}
