#' Calculates the thresholds
#' @keywords internal
#' \code{thresholds} Calculates the thresholds
#' @param qp output from quickpsy
#' @param prob Probability to calculate the threshold.
#' @param log Use \code{TRUE}, if the logarithm of the independent variable
#' has been used to fit the curves (default is \code{FALSE}).
#' @export

thresholds <- function(qp, prob){
  one_threshold <- function(par, curves, prob, log, funname,
                            guess, lapses) {
    if (funname %in%  names(get_functions())) {
      par <- par$par
      if (is.numeric(guess) && is.numeric(lapses))
        q <- (prob - guess) / (1 - guess - lapses)
      if (is.logical(guess) && is.logical(lapses))
        q <- (prob - par[3]) / (1 - par[3] - par[4])
      if (is.logical(guess) && is.numeric(lapses))
        q <- (prob - par[3]) / (1 - par[3] - lapses)
      if (is.numeric(guess) && is.logical(lapses))
        q <- (prob - guess) / (1 - guess - par[3])

      if (q < 0 || q > 1) {
        thre <- approx(curves$y,curves$x, xout = prob)$y
      }
      else {
        if (funname == "cum_normal_fun")
          thre <- inv_cum_normal_fun(q, c(par[1], par[2]))
        if (funname == "logistic_fun")
          thre <- inv_logistic_fun(q, c(par[1], par[2]))
        if (funname == "weibull_fun")
          thre <- inv_weibull_fun(q, c(par[1], par[2]))
      }
    }
    else {
      thre <- approx(curves$y,curves$x, xout = prob)$y
    }
    if (log) thre <- exp(thre)

    tibble(thre, prob)
  }

  if (is.null(prob)) {
    if (is.logical(qp$guess) && qp$guess) prob <- .5
    else  prob <- qp$guess + .5 * (1 - qp$guess)
  }

  apply_to_two_elements(qp,
                        par, curves,
                        ~one_threshold(.x, .y,
                                       prob,
                                       qp$log,
                                       qp$funname,
                                       qp$guess, qp$lapses))


}
