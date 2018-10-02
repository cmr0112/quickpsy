library(MPDiR)
library(tidyverse)
library(quickpsy)

### create data
pses <- c(1, 2, 3)
slopes <- c(.3, .6, .9)

dat <- crossing(participant = 1:3, size = c("large", "small")) %>%
  bind_cols(pse = c(.5, 0.7, 1, 1.5, 1.4, 1.6),
            slope = c(.2, .4, .4, .8, .6, .3)) %>%
  crossing(xx = seq(0, 2, .2)) %>%
  mutate(p = pnorm(xx, pse, slope)) %>%
  rowwise() %>%
  mutate(n = 50,
         k = rbinom(1, n, p),
         prob = k / n) %>%
  ungroup()

system.time(
  fit <- quickpsy(dat, xx, k, n,
                            grouping = .(participant, size),
                            prob = .6, bootstrap = "none")
  )
fit


### single data
dat1 <- dat %>%
  filter(participant == 1, size == "small") %>%
  select(-participant, -size)

system.time(fit1 <- quickpsy(dat1, xx, k, n, bootstrap = "none"))


glm(cbind(k,n) ~ xx, data =dat1, family = binomial(probit)) %>% logLik()

nll <- function(p) { # negative log likelihood
  phi <- pnorm(fit1$averages$xx, p[1], p[2])
  -sum(lchoose(fit1$averages$n, fit1$averages$k) +
         fit1$averages$k * log(phi) + (fit1$averages$n - fit1$averages$k) * log(1 - phi) )
}

para <- optim(c(.7, .7), nll)$par

nll(para)

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = xx, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob,
                                          color = size))


