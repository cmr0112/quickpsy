#' @keywords internal
#' @export
par_tibble <- function(d, parini,
                        x, k, n,
                        psyfunguesslapses, funname,
                        pariniset,
                        guess, lapses) {

  nllfun <- create_nll(d, x, psyfunguesslapses)

  parini <- parini$par

  if (funname == "weibull_fun") {
    if (parini[1] < 0) parini[1] <- .Machine$double.eps
    if (parini[2] < 0) parini[2] <- .Machine$double.eps

    if (guess | lapses) {
      lower <- c(parini[1], parini[2])
      upper <- c(Inf, Inf)
      if (guess) {
        lower <- c(lower, 0)
        upper <- c(upper, 1)
      }
      if (lapses) {
        lower <- c(lower, 0)
        upper <- c(upper, 1)
      }
      para <- optim(parini, nllfun, method = "L-BFGS-B",
                    lower = lower, upper = upper)$par
    }
    else {
      para <- optim(parini, nllfun)$par
    }
  }

  if (funname == "cum_normal_fun") {
    if (parini[2] < 0) parini[2] <- .Machine$double.eps

    if (guess | lapses) {
      lower <- c(-Inf, .Machine$double.eps)
      upper <- c(Inf, Inf)
      if (guess) {
        lower <- c(lower, 0)
        upper <- c(upper, 1)
      }
      if (lapses) {
        lower <- c(lower, 0)
        upper <- c(upper, 1)
      }
      para <- optim(parini, nllfun, method = "L-BFGS-B",
                      lower = lower, upper = upper)$par
      }
      else {
         para <- optim(parini, nllfun)$par
      }
    }
  tibble(parn = paste0('p', seq(1, length(para))), par = para)
}
