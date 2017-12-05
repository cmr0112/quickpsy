library(quickpsy)
library(MPDiR) #contains HSP
library(dplyr)


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


