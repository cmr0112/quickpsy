library(tidyverse)
n <- 100
x <- c(.2, .4, .6, .8, 1)
k <- c(10, 26, 73, 94, 97)
y <- k/n

dat <- crossing(cond = 1:400, tibble(x, y, n)) %>%
  group_by(cond) %>%
  mutate(k = rbinom(n(), n, y))


calculate_par <- function(df) {
  nll <- function(p, d) {
    phi <- pnorm(d$x, p[1], p[2])
    -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi) )
  }
  p <- optim(c(.7, .7), nll, d = df)$par
  tibble(par = p)
}

dat_group <- dat %>%
  group_by(cond) %>%
  nest()


dat_group_par <- dat_group %>%
mutate(par = map(data, calculate_par)) %>%
unnest(par)



calculate_par_no_map <- function(x, k, n) {
  nll <- function(p, d) {
    phi <- pnorm(d$x, p[1], p[2])
    -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi) )
  }
  df <- tibble(x,k,n)
  p <- optim(c(.7, .7), nll, d = df)$par
  p[[1]]
}

system.time(
dat_without_map <- dat %>%
  group_by(cond) %>%
  mutate(p = calculate_par_no_map(x, k, n))
)

p <- ggplot()+
  facet_wrap(~cond) +
  geom_point(data=dat,aes(x=x,y=y))
p


nll <- function(p, d) {
  phi <- pnorm(d$x, p[1], p[2])
  -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi) )
}

para <- optim(c(.7, .7), nll, d = dat)$par # we n
