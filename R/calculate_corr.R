calculate_corr <- function(d, x, y, ...){
  x <- enquo(x)
  y <- enquo(y)
  
  d %>% 
    group_by(...) %>% 
    group_map(~cor.test(.x %>% pull(!!x), 
                        .x %>% pull(!!y)) %>% 
                tidy()) 
}