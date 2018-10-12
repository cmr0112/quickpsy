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

### single data
dat1 <- dat %>%
  filter(participant == 1, size == "small") %>%
  select(-participant, -size)

pini <- c(1, 1)
pini <- list(c(.2, .3), c(.1, .6))
pini <- tibble(parn = c("p1", "p2"), par = c(1, 1))
pini <- tibble(parn = c("p1", "p2"), parmin = c(.2, .1), parmax = c(.3, .6))

fit1 <- quickpsy(dat1, xx, k, n, bootstrap = "none")

fit1$nll_fun$nll_fun[[1]](c(1, 1))

ggplot(dat1) +
  geom_point(aes(x = xx, y = prob)) +
  geom_line(data = fit1$curves, aes(x = x, y = y)) +
  geom_segment(data = fit1$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob)) +
  geom_segment(data = fit1$thresholds, aes(x = threinf, y = prob,
                                           xend = thresup,
                                           yend = prob))

### fit same slope
cum_normal_fun <- function(x, p) suppressWarnings(pnorm(x, p[1], p[2]))
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

fit <- quickpsy(dat, xx, k, n,
                grouping = .(participant, size),
                parini = pini,
                fun = fun_df,
                bootstrap = "none")

fit$nll_fun$nll_fun[[1]](c(1,1,1))

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = xx, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                           xend = thre,
                                           yend = prob,
                                          color = size))

### fit same slope one participant
cum_normal_fun <- function(x, p) suppressWarnings(pnorm(x, p[1], p[2]))
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

fit1 <- quickpsy(dat %>% filter(participant == 1), xx, k, n,
               grouping = .(size),
                parini = pini %>% filter(participant == 1),
                fun = fun_df,
                bootstrap = "none")

fit1$nll_fun$nll_fun[[1]](c(1,1,1))

ggplot(dat %>% filter(participant == 1)) +
  geom_point(aes(x = xx, y = prob, color = size)) +
  geom_line(data = fit1$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit1$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob,
                                          color = size))



### fit same pse
cum_normal_fun <- function(x, p) suppressWarnings(pnorm(x, p[1], p[2]))
cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[1], p[3]))

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2))
#fun_df <- cum_normal_fun

# pini <- crossing(participant = 1:3,
#                  parn = c("p1", "p2", "p3")) %>%
#   mutate(par = 2) %>%
#   group_by(participant)


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

### fit dif pse dif slope
cum_normal_fun <- function(x, p) suppressWarnings(pnorm(x, p[1], p[2]))
cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[4]))

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2))
#fun_df <- cum_normal_fun

pini <- crossing(participant = 1:3,
                 parn = c("p1", "p2", "p3", "p4")) %>%
  mutate(par = 2) %>%
  group_by(participant)


 pini <- c(1, 1, 1, 1)
 pini <- list(c(0, 2), c(0, 2), c(0, 2), c(0, 2))


fit <- quickpsy(dat, x, k, n,
                grouping = .(participant, size),
                parini = pini,
                fun = fun_df)

fit$nll_fun$nll_fun[[1]](c(1,1,1,1))

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size))+
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob,
                                          color = size))


### fit dif pse dif slope: single function
system.time(fit <- quickpsy(dat, xx, k, n,
                grouping = .(participant, size),
                prob = .6, B = 30))


vomggplot() +
  facet_grid(.~participant) +
  geom_point(data = fit$averages, aes(x = xx, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds,
               aes(x = thre, xend = thre, y = 0, yend = prob,
                   color = size))









  +
  geom_line(data = tibble(x = xseq, y = yseq1), aes(x = x, y = y))


+
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                xend = thre,
                                yend = prob,
                                color = size))


p <- fit$par %>%
  filter(participant == 2, size == "large") %>%
  pull(par)

xseq <- seq(0, 2, .01)
yseq1 <- cum_normal_fun(xseq, p)






### fit same slope
cum_normal_fun <- function(x, p) suppressWarnings(pnorm(x, p[1], p[2]))
cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[2]))

# pini <- crossing(participant = 1:3,
#                  parn = c("p1", "p2", "p3")) %>%
#   mutate(par = 2) %>%
#   group_by(participant)

pini <- crossing(participant = 1:3, size = c("small", "large"),
                 parn = c("p1", "p2", "p3")) %>%
  mutate(par = 2) %>%
  group_by(participant)

pini <- c(1, 1)
#pini <- list(c(0, 2), c(0, 2), c(0, 2))

fun_df <- tibble(size = c("large", "small"),
                  fun = c(cum_normal_fun, cum_normal_fun2))

fun_df <- cum_normal_fun

fit <- quickpsy(dat, x, k, n,
                grouping = .(participant, size),
                parini = pini,
                fun = fun_df)

fit$nll_fun$nll_fun[[1]](c(1,1,1))

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = fit$ypred, aes(x = x, y = y, color = size)) #+
  # geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
  #                                         xend = thre,
  #                                         yend = prob,
  #                                         color = size))






# parmin y parmax
pini <- list(c(0, 2), c(0, 2), c(0, 2))

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

fit <- quickpsy(dat, x, k, n,
                grouping = .(participant, size),
                parini = par_df, fun = fun_df)

fit$nll_fun$nll_fun[[1]](c(1,1,1))

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob,
                                          color = size))


### fit same pse
cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[1], p[3]))

pini <- list(c(0, 2), c(0, 2), c(0, 2))

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

fit <- quickpsy(dat, x, k, n, grouping = .(participant, size),
                parini = par_df, fun = fun_df)

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob,
                                          color = size))

### same slope and pse
cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[1], p[2]))

par_df <- crossing(participant = 1:3,
                   parn = c("p1", "p2")) %>%
  mutate(par = 1) %>%
  group_by(participant)

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

fit <- quickpsy(dat, x, k, n, grouping = .(participant, size),
                parini = par_df, fun = fun_df)

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob,
                                          color = size))

### different slope and pse
cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[4]))

par_df <- crossing(participant = 1:3,
                   parn = c("p1", "p2", "p3", "p4")) %>%
  mutate(par = 1) %>%
  group_by(participant)

par_df = c(1,1,1)

fun_df <- tibble(size = c("large", "small"),
                 psych_fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

fit <- quickpsy(dat, x, k, n, grouping = .(participant, size),
                parini = par_df, fun = fun_df)

ggplot(dat) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = fit$curves, aes(x = x, y = y, color = size)) +
  geom_segment(data = fit$thresholds, aes(x = thre, y = 0,
                                          xend = thre,
                                          yend = prob,
                                          color = size))




### dat12
dat12 <- dat %>%
  filter(participant %in% c(1, 2)) %>%
  ungroup()

cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[2]))

par_df <- crossing(participant = 1:2,
                   parn = c("p1", "p2", "p3")) %>%
  mutate(par = 1) %>%
  group_by(participant)

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

fit <- quickpsy(dat12, x, k, n,
                grouping = .(participant, size),
                parini = par_df,
                fun = fun_df)

p <- fit$par %>% filter(participant == 2) %>% pull(par)

xseq <- seq(0, 2, .01)
yseq1 <- cum_normal_fun(xseq, p)
yseq2 <- cum_normal_fun2(xseq, p)

ggplot(dat12) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = tibble(xseq, yseq1),
            aes(x = xseq, y = yseq1)) +
  geom_line(data = tibble(xseq, yseq2),
            aes(x = xseq, y = yseq2), color = "red")

### dat12 4 parameters

cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[4]))

par_df <- crossing(participant = 1:2,
                   parn = c("p1", "p2", "p3", "p4")) %>%
  mutate(par = 1) %>%
  group_by(participant)

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

fit <- quickpsy(dat12, x, k, n,
                grouping = .(participant, size),
                parini = par_df,
                fun = fun_df)

p <- fit$par %>% filter(participant == 2) %>% pull(par)

xseq <- seq(0, 2, .01)
yseq1 <- cum_normal_fun(xseq, p)
yseq2 <- cum_normal_fun2(xseq, p)

ggplot(dat12) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = tibble(xseq, yseq1),
            aes(x = xseq, y = yseq1)) +
  geom_line(data = tibble(xseq, yseq2),
            aes(x = xseq, y = yseq2), color = "red")


### dat1
dat1<- dat %>%
  filter(participant %in% c(2)) %>%
  ungroup()

cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[2]))

par_df <- crossing(participant = 2,
                   parn = c("p1", "p2", "p3")) %>%
  mutate(par = 1)

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

fit <- quickpsy(dat1, x, k, n,
                grouping = .(size),
                parini = par_df,
                fun = fun_df)


p <- fit$par %>% pull(par)

xseq <- seq(0, 2, .01)
yseq1 <- cum_normal_fun(xseq, p)
yseq2 <- cum_normal_fun2(xseq, p)

ggplot(dat1) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = tibble(xseq, yseq1),
            aes(x = xseq, y = yseq1)) +
  geom_line(data = tibble(xseq, yseq2),
            aes(x = xseq, y = yseq2), color = "red")






create_nll <- function(d, fun_df, x) {
 # x <- enquo(x)

    calculate_nll <- function(d, fun) {
      x <- d %>% select(!!x) %>% pull()
      eps <- .Machine$double.eps

      phi <- fun(x, p)
      phi[phi < eps] <- eps
      phi[phi > (1 - eps)] <- 1 - eps

      -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
    }

    # z <- d %>%
    #   ungroup() %>% # a lo mejor no hace falta
    #   group_by_at(group_vars(fun_df)) %>%
    #   nest() %>%
    #   left_join(fun_df, by = group_vars(fun_df)) %>%
    #   mutate(nll = map2_dbl(data, fun, calculate_nll))
    #
    # print(z)

    # %>%
    #
    #    %>%
    #    %>%
    #   summarise(nll = sum(nll))

  function(p) {
    calculate_nll <- function(d, fun) {
      x <- d %>% select(!!x) %>% pull()
      eps <- .Machine$double.eps

      phi <- fun(x, p)
      phi[phi < eps] <- eps
      phi[phi > (1 - eps)] <- 1 - eps

      -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
    }

    d %>%
      ungroup() %>% # a lo mejor no hace falta
      group_by_at(group_vars(fun_df)) %>%
      nest() %>%
      left_join(fun_df, by = group_vars(fun_df)) %>%
      mutate(nll = map2_dbl(data, fun, calculate_nll)) %>%
      summarise(nll = sum(nll))

  }
}

av <- fit$averages %>%
  filter(participant == 1)

nll <- create_nll(av, fun_df, x)

nll(c(1,1,1))

p <- optim(c(1,1,1), nll)$p

xseq <- seq(0, 2, .01)
yseq1 <- cum_normal_fun(xseq, p)
yseq2 <- cum_normal_fun2(xseq, p)

ggplot(dat12) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = tibble(xseq, yseq1),
            aes(x = xseq, y = yseq1)) +
  geom_line(data = tibble(xseq, yseq2),
            aes(x = xseq, y = yseq2), color = "red")


nll_fun <- function(averages, fun, x) {

  groups <- group_vars(averages) %>% setdiff(group_vars(fun))

  averages_df <- averages %>%
    group_by_at(vars(groups)) %>%
    nest(.key = averages)
  averages_df %>%
    mutate(fun = list(fun_df)) %>%
    mutate(nll_fun = map2(averages, fun, ~create_nll(.x, .y, x))) %>%
    group_by_at(vars(groups)) %>%
    select(-averages, -fun)
}

n <- nll_fun(fit$averages, fun_df, x)


nll <- n$nll_fun[[1]]

nll(c(1,1,1))

p <- optim(c(1,1,1), nll)$p

xseq <- seq(0, 2, .01)
yseq1 <- cum_normal_fun(xseq, p)
yseq2 <- cum_normal_fun2(xseq, p)

ggplot(dat12) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = tibble(xseq, yseq1),
            aes(x = xseq, y = yseq1)) +
  geom_line(data = tibble(xseq, yseq2),
            aes(x = xseq, y = yseq2), color = "red")





### dat 1 large
dat1l <- dat %>% filter(participant == 1, size == "large")

ggplot(dat1l) +
  geom_point(aes(x = x, y = prob)) +
  geom_line(aes(x = x, y = prob))

create_nll <- function(d, psyfunguesslapses){
#create_nll <- function(d, x, psyfunguesslapses){
  function(p) {
    #x <- d %>% select(!!x) %>% pull()
    x <- d %>% select(x) %>% pull()
    eps <- .Machine$double.eps

    phi <- psyfunguesslapses(x, p)
    phi[phi < eps] <- eps
    phi[phi > (1 - eps)] <- 1 - eps

    -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
  }
}

nll <- create_nll(dat1l, cum_normal_fun)

nll(c(1,1))

p <- optim(c(1, 1), nll)$p

xseq <- seq(0, 2, .01)
yseq <- cum_normal_fun(xseq, p)

ggplot(dat1l) +
  geom_point(aes(x = x, y = prob)) +
  geom_line(data = tibble(xseq, yseq),
            aes(x = xseq, y = yseq))

### dat 1
dat1 <- dat %>%
  filter(participant == 1)

cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[2]))

formalArgs(cum_normal_fun)

fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

ggplot(dat1) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(aes(x = x, y = prob, color = size))

create_nll <- function(d, fun_df) {
    #x <- d %>% select(!!x) %>% pull()
  function(p) {
    calculate_nll <- function(d, fun) {
      x <- d %>% select(x) %>% pull()
      eps <- .Machine$double.eps

      phi <- fun(x, p)
      phi[phi < eps] <- eps
      phi[phi > (1 - eps)] <- 1 - eps

      -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
    }

    d %>%
      ungroup() %>% # a lo mejor no hace falta
      group_by_at(group_vars(fun_df)) %>%
      nest() %>%
      left_join(fun_df, by = group_vars(fun_df)) %>%
      mutate(nll = map2_dbl(data, fun, calculate_nll)) %>%
      summarise(nll = sum(nll))

  }
}

nll <- create_nll(dat1, fun_df)

nll(c(1,2,2))

p <- optim(c(1, 1,1), nll)$p

xseq <- seq(0, 2, .01)
yseq1 <- cum_normal_fun(xseq, p)
yseq2 <- cum_normal_fun2(xseq, p)

ggplot(dat1) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = tibble(xseq, yseq1),
            aes(x = xseq, y = yseq1)) +
  geom_line(data = tibble(xseq, yseq2),
            aes(x = xseq, y = yseq2), color = "red")

### dat 12
dat12 <- dat %>%
  filter(participant %in% c(1, 2)) %>%
  ungroup()

ggplot(dat12) +
  facet_grid(. ~ participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(aes(x = x, y = prob, color = size))










param <- function(nll_fun, parini) {
  calculate_par <- function(parini, nll_fun) {
    par <- optim(parini$par, nll_fun)$p
    tibble(parn = paste0('p', seq(1, length(par))), par = par)
  }

  parini %>%
    group_by_at(vars(group_vars(nll_fun))) %>%
    nest(.key = "parini") %>%
    left_join(nll_fun, by = group_vars(nll_fun)) #%>%
    #mutate(par = map2(parini, nll_fun, calculate_par)) #%>%
    #select(-nll_fun, -parini) %>%
    #unnest(par)
}

pini <- para$parini[[1]]
nll <- para$nll_fun[[1]]
calculate_par(pini, nll)

optim(pini$par, nll)$p

para <- param(nll_fun, parini)
para

nll_fun <- fit$nll_fun
parini <- par_df





#########
create_nll <- function(d, fun_df, x) {
  function(p) {
    calculate_nll <- function(d, fun) {
      x <- d %>% select(!!x) %>% pull()
      eps <- .Machine$double.eps

      phi <- fun(x, p)
      phi[phi < eps] <- eps
      phi[phi > (1 - eps)] <- 1 - eps

      -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
    }

    group_vars(fun_df) %>% print()

    x <- d %>%
      ungroup() %>% # a lo mejor no hace falta
      group_by_at(group_vars(fun_df))

    y <- x %>%
      nest() %>%
      left_join(fun_df, by = group_vars(fun_df)) %>%
      mutate(nll = map2_dbl(data, fun, calculate_nll)) %>%
      summarise(nll = sum(nll))

    y

  }
}




par <- fit$par

par <- function(nll_fun, parini) {
  calculate_par <- function(parini, nll_fun) {
    par <- optim(parini$par, nll_fun)$p
    tibble(parn = paste0('p', seq(1, length(par))), par = par)
  }

  parini %>%
    group_by_at(vars(group_vars(nll_fun))) %>%
    nest(.key = "parini") %>%
    left_join(nll_fun, by = group_vars(nll_fun)) %>%
    mutate(par = map2(parini, nll_fun, calculate_par))
}

nll <- fit$nll_fun$nll_fun[[1]]








ggplot(dat1) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(aes(x = x, y = prob, color = size))

create_nll <- function(d, fun_df) {
  #x <- d %>% select(!!x) %>% pull()
  function(p) {
    calculate_nll <- function(d, fun) {
      x <- d %>% select(x) %>% pull()
      eps <- .Machine$double.eps

      phi <- fun(x, p)
      phi[phi < eps] <- eps
      phi[phi > (1 - eps)] <- 1 - eps

      -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
    }

    d %>%
      ungroup() %>% # a lo mejor no hace falta
      group_by_at(group_vars(fun_df)) %>%
      nest() %>%
      left_join(fun_df, by = group_vars(fun_df)) %>%
      mutate(nll = map2_dbl(data, fun, calculate_nll)) %>%
      summarise(nll = sum(nll))

  }
}

nll <- create_nll(dat1, fun_df)

nll(c(1,2,2))

p <- optim(c(1, 1,1), nll)$p

xseq <- seq(0, 2, .01)
yseq1 <- cum_normal_fun(xseq, p)
yseq2 <- cum_normal_fun2(xseq, p)

ggplot(dat12) +
  facet_grid(.~participant) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = tibble(xseq, yseq1),
            aes(x = xseq, y = yseq1)) +
  geom_line(data = tibble(xseq, yseq2),
            aes(x = xseq, y = yseq2), color = "red")





### dat 1
dat1 <- dat %>% filter(participant == 1)


fun_df <- tibble(size = c("large", "small"),
                 fun = c(cum_normal_fun, cum_normal_fun2)) %>%
  group_by(size)

parini <- tibble(size = c(large, large, small, small),
                 parn =c(" "))

fit <- quickpsy(dat1, x, k, n, grouping = .(size),
                parini = c(1, 1, 1), fun = fun_df)

fit

cum_normal_fun2 <- function(x, p) suppressWarnings(pnorm(x, p[3], p[2]))

formalArgs(cum_normal_fun)


ggplot(dat1) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(aes(x = x, y = prob, color = size))

create_nll <- function(d, fun_df) {
  #x <- d %>% select(!!x) %>% pull()
  function(p) {
    calculate_nll <- function(d, fun) {
      x <- d %>% select(x) %>% pull()
      eps <- .Machine$double.eps

      phi <- fun(x, p)
      phi[phi < eps] <- eps
      phi[phi > (1 - eps)] <- 1 - eps

      -sum(d$k * log(phi) + (d$n - d$k) * log(1 - phi))
    }

    d %>%
      ungroup() %>% # a lo mejor no hace falta
      group_by_at(group_vars(fun_df)) %>%
      nest() %>%
      left_join(fun_df, by = group_vars(fun_df)) %>%
      mutate(nll = map2_dbl(data, fun, calculate_nll)) %>%
      summarise(nll = sum(nll))

  }
}

nll <- create_nll(dat1, fun_df)

nll(c(1,2,2))

p <- optim(c(1, 1,1), nll)$p

xseq <- seq(0, 2, .01)
yseq1 <- cum_normal_fun(xseq, p)
yseq2 <- cum_normal_fun2(xseq, p)

ggplot(dat1) +
  geom_point(aes(x = x, y = prob, color = size)) +
  geom_line(data = tibble(xseq, yseq1),
            aes(x = xseq, y = yseq1)) +
  geom_line(data = tibble(xseq, yseq2),
            aes(x = xseq, y = yseq2), color = "red")










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





