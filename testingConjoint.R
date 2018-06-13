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

### dat 1 large
dat1l <- dat %>% filter(participant == 1, size == "large")

ggplot(dat1) +
  geom_point(aes(x = x, y = prob)) +
  geom_line(aes(x = x, y = prob))

create_nll <- function(d, x, psyfunguesslapses){
  function(p) {
    x <- d %>% select(!!x) %>% pull()
    eps <- .Machine$double.eps

    phi <- psyfunguesslapses(x, p)
    phi[phi < eps] <- eps
    phi[phi > (1 - eps)] <- 1 - eps

    -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
  }
}







conditions <- dat %>% distinct(participant, size)

fun1 <- function(x, p) pnorm(x, p[1], p[2])
fun2 <- function(x, p) pnorm(x, p[3], p[2])

conditions_size <- dat %>% distinct(size)

par_ini <- c(1, 1, 0.5)

df_fun <- conditions_size %>%
  bind_cols(tibble(fun = c(fun1, fun2)))

fit <- quickpsy(dat, x, k, n,
                grouping = .(participant, size),
                fun = df_fun,
                parini = par_ini)

fit$conditions_conjoint

ggplot(fit$averages) +
  facet_grid(. ~ participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(aes(x = x, y = prob, color = size))

######
averages <-fit$averages
parini <- par_ini
fun <- fit$fun

nll <- function(averages, fun) {
  groups_conjoint <- setdiff(groups(averages), groups(fun))
}

parametersNew <- function(averages, parini, fun) {
  groups_conjoint <- setdiff(groups(averages), groups(fun))

  k <- averages %>%
    group_by(!!!groups_conjoint)

  k


}

para <- parametersNew(averages, parini, fun)





