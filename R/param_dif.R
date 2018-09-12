#' @keywords internal
#' @export param_dif
param_dif <- function(param) {

  parn <- param %>% ungroup() %>% distinct(parn)

  combinations <- as_tibble(t(combn(nrow(param), 2)))

  print(combinations)

  select_rows <- function(V1, V2) {
    cond1 <- param[V1,]
    cond2 <- param[V2,]
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
