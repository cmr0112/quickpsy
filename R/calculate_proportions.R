calculate_proportions <- function(d, resp, ...) {
  resp <- enquo(resp)
  
  d %>%
    group_by(...) %>%
    summarise(n = n(), k = sum(!!resp),
              r = n - k,
              prob = mean(!!resp),
              p.value = binom.test(k, n)$p.value,
              signif = if_else(p.value < .05, TRUE, FALSE),
              ymin = binom.test(k, n)$conf.int[1],
              ymax = binom.test(k, n)$conf.int[2]
    )
}