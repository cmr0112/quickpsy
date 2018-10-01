#' @keywords internal
#' @export

quickpsy_without_bootstrap <- function(d, x, k, n,
                     groups,
                     xmin, xmax,
                     log,
                     fun,
                     funname,
                     parini,
                     guess, lapses,
                     prob, thresholds) {


  ### Calling functions
  averages <- averages(d, x, k, n, groups, log)

  limits <- limits(averages, x, xmin, xmax)

  psych_fun <- psych_fun(fun, guess, lapses)

  nll_fun <- nll_fun(averages, psych_fun, x, create_nll)

  nll_fun_saturated <- nll_fun(averages, psych_fun, x, create_nll_saturated)

  if (is.null(parini) & (funname %in% names(get_functions()))) {
    parini <- calculate_parini(averages, funname, x, guess, lapses)
  }
  else if (is.null(parini) & !(funname %in% names(get_functions()))){
    stop("parini (initial parameters) must be specified.", call. = FALSE)
  }
  else {
    parini <- parini(averages, parini, psych_fun)
  }

  param <- param(nll_fun, parini)

  ypred <- ypred(averages, param, psych_fun, x, log)

  x_seq <- x_seq(limits, x)

  curves <- ypred(x_seq, param, psych_fun, x, log)

  sse <-  sse(averages, ypred)

  logliks <- logliks(nll_fun, param)

  loglikssaturated <- logliks_saturated(nll_fun_saturated, averages)

  aic <- akaike(logliks, param)

  deviance <- devi(logliks, loglikssaturated)

  qp <- list(averages = averages,
             limits = limits,
             psych_fun = psych_fun,
             nll_fun = nll_fun,
             nll_fun_saturated = nll_fun_saturated,
             parini = parini,
             par = param,
             ypred = ypred,
             x_seq = x_seq,
             curves = curves,
             sse = sse,
             logliks = logliks,
             loglikssaturated = loglikssaturated,
             aic = aic,
             deviance = deviance)


  if (length(groups(averages)) != 1) {
    param_dif <- param_dif(param)
    qp <- c(qp, list(par_dif = param_dif))
  }

  if (thresholds) {
    thresholds <- thresholds(param, curves, psych_fun, prob, log, guess, lapses)
    qp <- c(qp, list(thresholds = thresholds))

    if (length(groups(averages)) != 1) {
      thresholds_dif <- thresholds_dif(thresholds)
      qp <- c(qp, list(thresholds_dif = thresholds_dif))
    }
  }

  qp

}

