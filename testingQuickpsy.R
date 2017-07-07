library(quickpsy)
library(MPDiR) #contains HSP

HSP <- HSP %>% mutate(k = round(N * p / 100)) # to calculate the number of 'yes'
fit <- quickpsy(HSP, Q, k, N, prob = .6, grouping = .(Run, Obs), log = T, B = 5)


gompertz_fun <- function(x, p) exp(-p[1] * exp(-p[2] * x))
fit <- qpdat %>% quickpsy(phase, resp, fun = gompertz_fun, parini = c(1,.01),
                          grouping = .(interval), B = 5)
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


