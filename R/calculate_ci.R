calculate_ci <- function(d, x, ...) {
  d %>%
    group_by(...) %>%
    group_map(~t.test(.x %>% pull(!!enquo(x)), conf.int = TRUE) %>% tidy()) 
}
