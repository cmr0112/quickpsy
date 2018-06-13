library(quickpsy)
library(MPDiR) #contains HSP
library(dplyr)

pses <- c(1, 2, 3)
slopes <- c(.3, .6, .9)

dat <- crossing(participant = 1:3, size = c("large", "small")) %>%
  bind_cols(pse = c(.5, 0.7, 1, 1.2, 1.4, 1.6),
            slope= c(.2, .2, .4, .4, .6, .6)) %>%
  crossing(x = seq(0, 2, .2)) %>%
  mutate(p = pnorm(x, pse, slope)) %>%
  rowwise() %>%
  mutate(n = 50,
         k = rbinom(1, n, p),
         prob = k / n)

ggplot(dat) +
  facet_grid(. ~ participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(aes(x = x, y = prob, color = size))

conditions <- dat %>% distinct(participant, size)

fun1 <- function(x, p) pnorm(x, p[1], p[2])
fun2 <- function(x, p) pnorm(x, p[3], p[2])

conditions_size <- dat %>% distinct(size)

par_ini <- c(1, 1, 0.5)

df_fun <- conditions_size %>%
  bind_cols(tibble(fun = c(fun1, fun2)))

fit <- quickpsy(dat, x, k, n, grouping = .(participant, size), fun = df_fun)

fit$limits
ggplot(fit$averages) +
  facet_grid(. ~ participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(aes(x = x, y = prob, color = size))







HSP <- HSP %>% mutate(k = round(N * p / 100)) # to calculate the number of 'yes'
fit <- quickpsy(HSP, Q, k, N, prob = .6, grouping = .(Run, Obs), log = TRUE, B = 5)

ggplot() +
  facet_wrap(~Obs) +
  geom_point(data = fit$averages, aes(x = Q, y = prob, color = Run)) +
  geom_line(data = fit$curves, aes(x = log(x), y = y, color = Run))

HSP <- HSP %>% mutate(k = round(N * p / 100)) %>%
  filter(Obs == "SH", Run == "R1") %>%
  mutate(logQ = log(Q))


fit <- quickpsy(HSP, Q, k, N, prob = .6, log = T, B = 5)

ggplot() +
  geom_point(data = fit$averages, aes(x = Q, y = prob)) +
  geom_line(data = fit$curves, aes(x = log(x), y = y))


dat <- read.table("test.txt", header = TRUE)

fit <- quickpsy(dat, phase, resp, grouping=.(ecc, interval), B = 1)

gompertz_fun <- function(x, p) exp(-p[1] * exp(-p[2] * x))
fit <- qpdat %>% quickpsy(phase, resp, fun = gompertz_fun, parini = c(1,.01),
                          grouping = .(subject,interval), B = 5)

ggplot(fit$averages, aes(x = phase, y= prob, color = factor(interval))) +
  geom_point() +
  geom_line(data = fit$curves, aes(x = x, y= y))

fit %>% plot()

x <- seq(0, 420, 60)
k <- c(0, 0, 4, 18, 20, 20, 19, 20)
dat <- tibble(x, k, n = 20)
fitWithoutLapses <- quickpsy(dat, x, k, n, prob = .75)


functs <- Vernier %>% distinct(Direction) %>%
  bind_cols(tibble(fun = c(cum_normal_fun, function(x, p) x^p)))

fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
                grouping = .(Direction, WaveForm, TempFreq), B = 2)
fit %>% plot()

f1 <- function(x, p) p[1] * x + p[2]
f2 <- function(x, p) p[3] * x + p[4]
f <- function(x, p) f1 + f2


