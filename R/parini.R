#' Calculates the initial parameters
#' @keywords internal
#' @export parini
parini <- function(averages, x, guess, lapses, funname,
                   parini, pariniset, groups) {

  one_parini <- function(d, x, guess, lapses, psyfun) {

    ntrials <- d %>% select(n) %>% pull() %>% first()
    x <- d %>% select(!!x) %>% pull()
    y <- d %>% select(prob) %>% pull()

    if (is.numeric(guess) && is.numeric(lapses)) {
      gue <- guess
      lap  <- lapses
    }
    if (is.logical(guess) && is.logical(lapses)) {
      if (guess && lapses) {
        gue <- min(y)
        lap  <- 1 - max(y)
      }
    }
    if (is.logical(guess) && is.numeric(lapses)) {
      lap <- lapses
      if (guess) gue <- min(y)
      if (!guess) gue <- 0
    }
    if (is.numeric(guess) && is.logical(lapses)) {
      gue <- guess
      if (lapses) lap <- 1 - max(y)
      if (!lapses) lap <- 0
    }

    ### Transforming y values to be closer to the range (0,1)
    y01 <- (y - gue) / (1 - gue - lap)
    datp <- tibble(x = x, y01)

    ### Replacing 0s and/or 1s by 1 / (2 * n) and 1 - 1 / (2 * n)
    # where n is the number of trials
    datp <- datp %>%
      mutate(y01 = ifelse(y01 == 1, 1 - 1 / (2 * ntrials), y01)) %>%
      mutate(y01 = ifelse(y01 == 0, 1 / (2 * ntrials), y01))

    ### Eliminating probabilities outside (0,1)
    dat <- datp %>% filter(y01 > 0, y01 <1)

    ### Linear fit
    dat <- dat %>% mutate(z = qnorm(y01))

    coef <- lm(z~x, data = dat)$coefficients

    if (coef[[2]] == 0) { # checking that the slope is not zero
      p1 <- median(dat$x)
      p2 <- (1 - gue - lap) / (max(dat$x)-min(dat$x))
    }
    else {
      p1 <- -coef[[1]] / coef[[2]]
      p2 <- 1 / coef[[2]]
    }

    if (psyfun == 'logistic_fun') p2 <- 1 / p2
    if (psyfun == 'weibull_fun') p2 <- 1 / p2

    if (is.numeric(guess) && is.numeric(lapses)) para <- c(p1, p2)
    if (is.logical(guess) && is.logical(lapses)) {
      if (guess && lapses) para <- c(p1, p2, gue, lap)
      if (!guess && !lapses) para <- c(p1, p2)
    }
    if (is.logical(guess) && is.numeric(lapses)) {
      if (guess) para <- c(p1, p2, gue)
      if (!guess) para <- c(p1, p2)
    }
    if (is.numeric(guess) && is.logical(lapses)) {
      if (lapses) para <- c(p1, p2, lap)
      if (!lapses) para <- c(p1, p2)
    }
    tibble(parn = paste0('p', seq(1, length(para))), par = para)
  }

  if (!pariniset)
    if (funname %in% names(get_functions())) {
      parini <-   averages %>%
        nest(everything(), .key = averages) %>%
        mutate(parini = map(averages,
                            ~one_parini(.x, x,
                                        guess, lapses,
                                        funname))) %>%
        select(-averages) %>%
        unnest(parini) %>%
        group_by(!!!syms(groups))
    }
  else {
    stop("parini (initial parameters) must be specified.")
  }
  else {
    if (is.atomic(parini)) {
      parini <- tibble(parn = paste0('p', seq(1, length(parini))),
                       par = parini)
      parini <- expand(averages %>% distinct(!!!syms(groups)), parini)
    }
    else
      if(is.list(parini)) {
        parini <- matrix(unlist(parini), ncol = 2, byrow = TRUE) %>% as_tibble()
        names(parini) <- c("parmin", "parmax")
        parini <- tibble(parn = paste0("p", seq(1, nrow(parini)))) %>%
          bind_cols(parini)
        parini <- expand(averages %>% distinct(!!!syms(groups)), parini)
      }
  }
  parini
}
