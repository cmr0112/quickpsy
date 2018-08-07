library(MPDiR)
library(tidyverse)

### create data
pses <- c(1, 2, 3)
slopes <- c(.3, .6, .9)

dat <- crossing(participant = 1:3, size = c("large", "small")) %>%
  bind_cols(pse = c(.5, 0.7, 1, 1.5, 1.4, 1.6),
            slope= c(.2, .4, .4, .8, .6, .3)) %>%
  crossing(x = seq(0.2, 2, .2)) %>%
  mutate(p = pnorm(x, pse, slope)) %>%
  rowwise() %>%
  mutate(n = 50,
         k = rbinom(1, n, p),
         prob = k / n) %>%
  ungroup()

### fit same slope
cum_normal_fun <- function(x, p) suppressWarnings(pnorm(x, p[1], p[2]))
cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[2]))

par_df <- crossing(participant = 1:3,
                   parn = c("p1", "p2", "p3")) %>%
  mutate(par = 1) %>%
  group_by(participant)

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

fit <- quickpsy(dat, x, k, n,
                grouping = .(participant, size),
                parini = par_df, fun = fun_df, log = FALSE)

fit$nll_fun$nll_fun[[1]](c(1,1,1))

ggplot(fit$averages) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob,
                                          color = size))
