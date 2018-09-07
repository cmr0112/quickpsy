library(MPDiR)
library(tidyverse)
library(quickpsy)

### create data
pses <- c(1, 2, 3)
slopes <- c(.3, .6, .9)

dat <- crossing(participant = 1:3, size = c("large", "small")) %>%
  bind_cols(pse = c(.5, 0.7, 1, 1.5, 1.4, 1.6),
            slope = c(.2, .4, .4, .8, .6, .3)) %>%
  crossing(x = seq(0, 2, .2)) %>%
  mutate(p = pnorm(x, pse, slope)) %>%
  rowwise() %>%
  mutate(n = 50,
         k = rbinom(1, n, p),
         prob = k / n) %>%
  ungroup()

### single data
dat1 <- dat %>%
  filter(participant == 1, size == "small") %>%
  select(-participant, -size)

fit1 <- quickpsy(dat1, x, k, n)

ggplot(dat1) +
  geom_point(aes(x = x, y = prob)) +
  geom_line(data = fit1$curves, aes(x = x, y = y)) +
  geom_segment(data = fit1$thresholds, aes(x = thre, y = 0,
                                           xend = thre,
                                           yend = prob))


fit1$logliks
fit1$loglikssaturated
fit1$aic
fit1$deviance

fit1_glm <- glm(cbind(dat1$k, dat1$n - dat1$k) ~
                  dat1$x, family = binomial(probit))
fit1_glm
logLik(fit1_glm)
deviance(fit1_glm)

fit1 <- quickpsy(dat1, x, k, n, fun = logistic_fun)
fit1$logliks

fit1_glm <- glm(cbind(k, n - k) ~x, data = dat1, family = binomial(logit))

logLik(fit1_glm)
fit1$deviance



### fit same slope
cum_normal_fun <- functixon(x, p) suppressWarnings(pnorm(x, p[1], p[2]))
cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[2]))

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2))

# pini <- crossing(participant = 1:3,
#                    parn = c("p1", "p2", "p3")) %>%
#   mutate(par = 2) %>%
#   group_by(participant)

pini <- crossing(participant = 1:3,
                 parn = c("p1", "p2", "p3")) %>%
  mutate(parmin = 0, parmax = 2) %>%
  group_by(participant)


#pini <- c(1, 1, 1)
pini <- list(c(0, 2), c(0, 2), c(0, 2))

fit <- quickpsy(dat, x, k, n,
                grouping = .(participant, size),
                parini = pini,
                fun = fun_df)

fit$nll_fun$nll_fun[[1]](c(1,1,1))

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob,
                                          color = size))


fit$logliks
fit$aic
fit$deviance

models <- dat %>%
  group_by(participant) %>%
  nest() %>%
  mutate(model = map(data,
                     ~ glm(cbind(k, n - k) ~ x + size -1,
                           data = ., family = binomial(probit))),
         loglik = map_dbl(model, logLik),
         deviance = map_dbl(model, deviance))

models


### fit dif pse dif slope: single function
pini <- c(1, 1)

fit <- quickpsy(dat, x, k, n,
                grouping = .(participant, size),
                prob = .7,
                bootstrap = "k")

fit$nll_fun$nll_fun[[1]](c(1,1))

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds,
               aes(x = thre, xend = thre, y = 0, yend = prob,
                   color = size))

fit$logliks
fit$aic
fit$deviance


dat %>%
  group_by(participant, size) %>%
  nest() %>%
  mutate(model = map(data, ~ glm(cbind(k, n - k) ~ x, data = .,
                                 family = binomial(probit))),
         loglik = map_dbl(model, logLik),
         aic = map_dbl(model, AIC),
         deviance = map_dbl(model, deviance))


