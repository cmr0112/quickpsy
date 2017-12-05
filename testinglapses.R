library(tidyverse)
library(quickpsy)
dat <- read.table("dat.txt")

dat9 <- dat %>% filter(task == 'comp', subject == 9 | subject == 8 , vertical== FALSE)


fit <- quickpsy(dat9,
                     orSmall, response,
                     grouping = .(subject, orLarge),
                bootstrap = "parametric", # lapses = TRUE,
                #parini = c(0, 1, .1),
                #     guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                  #   parini = list(c(-2, 2), c(0.1,3), c(0,.4)),
                 #  parini = list(c(-2, 2), c(0.1,3)),
                     B = 10)

ggplot() + facet_grid(orLarge~subject) +
  geom_point(data = fit$averages, size = 3,
             aes(x = orSmall, y = prob)) +
  geom_point(data = fit$avbootstrap,  alpha = .1,
             aes(x = orSmall, y = prob,  group = sample)) +
   geom_line(data = fit$curves, aes(x = x, y = y, color = orLarge)) +
   geom_segment(data = fit$thresholds, aes(x = thre, xend = thre,
                                           y = 0, yend = prob,
                                           color = orLarge))

n <- 100
x <- c(.2, .4, .6, .8, 1) # luminance
k <- c(10, 26, 73, 94, 97) # number of times that the observer reports that can see the stimulus
y <- k/n # proportion
dat <- data.frame(x, y, k, n, cond = "cond")

fit <- quickpsy(dat, x, k, n, B = 3)
fit <- quickpsy(dat, x, k, n, B = 3, parini = list(c(0,1), c(0,1)))

ggplot() +
  geom_point(data = fit$averages, aes(x = x, y = prob)) +
  geom_line(data = fit$curves, aes(x = x, y = y))
 # geom_line(data = fit$ypred, aes(x = x, y = y))


fitWithoutLapses <- quickpsy(qpdat %>%
                               filter(participant == "bb",
                                      interval == 1600,
                                      ecc == 3), phase, resp,grouping = c(ecc, interval ,participant),
                             bootstrap = "none")

fitWithLapses <- quickpsy(qpdat %>%
                            filter(participant == "bb",
                                   interval == 1600,

                                   ecc == 3), phase, resp,
                          grouping = c(ecc, interval ,participant),
                          lapses = T, guess = T,
                           B = 2)
                          #parini = list(c(1, 1000), c(1,1000)))
ggplot() +
  geom_point(data = fitWithLapses$averages,
             aes(x = phase, y = prob, color = factor(ecc))) +
  geom_line(data = fitWithLapses$ypred,
            aes(x = x, y = y, color = factor(ecc)))
