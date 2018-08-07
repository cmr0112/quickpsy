#' Fits psychometric functions
#'
#' \code{quickpsy} fits, by direct maximization of the likelihood
#' (Prins and Kingdom, 2010; Knoblauch and Maloney, 2012),
#'  psychometric functions of the form
#' \deqn{\psi(x) = \gamma + (1 - \gamma - \lambda) * fun(x)}
#' where \eqn{\gamma} is the guess rate, \eqn{\lambda} is the lapse rate and
#' \eqn{fun} is a sigmoidal-shape function with asymppotes at 0 and 1.
#' @param d Data frame with the results of a Yes-No experiment to fit.
#' It should have a
#' \href{http://vita.had.co.nz/papers/tidy-data.html}{tidy} form in which
#' each column corresponds to a variable and each row is an observation.
#' @param x Name of the explanatory variable.
#' @param k Name of the response variable. The response variable could be the
#' number of trials in which a yes-type response was given or a vector of 0s
#' (or -1s; no-type response) and 1s (yes-type response) indicating the
#' response on each trial.
#' @param n Only necessary if \code{k} refers to the number of trials
#' in which a yes-type response was given. It corresponds to the name of the
#' variable indicating the total number of trials.
#' @param grouping Name of the grouping variables. It should be specified as
#' \code{grouping = .(variable_name1, variable_name2)}.
#' @param xmin Minimum value of the explanatory variable for which the curves
#' should be calculated (the default is the minimum value of the explanatory
#' variable).
#' @param xmax Maximum value of the explanatory variable for which the curves
#' should be calculated (the default is the maximum value of the explanatory
#' variable).
#' @param log If \code{TRUE}, the logarithm of the explanatory variable is used
#' to fit the curves (default is \code{FALSE}).
#' @param fun Name of the shape of the curve to fit. It could be a predefined
#' shape (\code{cum_normal_fun}, \code{logistic_fun}, \code{weibull_fun})
#' or the name of a function introduced by the user
#' (default is \code{cum_normal_fun}).
#' @param parini Initial parameters. quickpsy calculates default
#' initial parameters for the predefined functions by linear modelling of
#' the probit-transformed data. Otherwise, \code{parini} could be
#' \itemize{
#'   \item  a vector of initial parameters
#'   \item a list of the form
#'   \code{list(c(par1min, par1max), c(par2min, par2max))} to
#'   constraint the lower and upper bounds of the parameters
#'   \item a dataframe specifiying the initial parameters for each condition
#'   with the same structure that the output \code{par},
#'   but without the confidence intervals.
#'  }
#' @param guess Value indicating the guess rate \eqn{\gamma} (default is 0). If
#' \code{TRUE}, the guess rate is estimated as the i + 1 paramEter where
#' i corresponds to the number of parameters of \code{fun}. If, for
#' example, \code{fun} is a predefined shape with parameters p1 and p2,
#' then the guess rate corresponds to parameter p3.
#' @param lapses Value indicating the lapse rate \eqn{\lambda} (default is 0).
#'  If \code{TRUE}, the lapse rate is estimated as the i + 1 parameter where
#' i corresponds to the number of parameters of \code{fun} plus one if
#' the guess rate is estimated. If, for example, \code{fun} is a
#' predefined shape with parameters p1 and p2,
#' then the lapse rate corresponds to parameter p3. If the guess rate is also
#' estimated, p3 will be the guess rate and p4 the lapse rate.
#' @param prob Probability to calculate the threshold (default is
#' \code{guess + .5 * (1 - guess)}).
#' @param thresholds If \code{FALSE}, thresholds are not calculated
#' (default is \code{TRUE}).
#' @param bootstrap \code{'parametric'} performs parametric bootstrap;
#' \code{'nonparametric'} performs non-parametric bootstrap;
#' \code{'none'} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is 100 ONLY).
#' @param ci Confidence intervals level based on percentiles (default is .95).
#' @param optimization Method used for optimizization. The default is 'optim' which uses
#' the \code{optim} function. It can also be \code{'DE'} which uses de function
#' \code{DEoptim} from the package DEoptim, which performs differential
#' evolution optimization. By using \code{DEoptim}, it is less likely that the
#' optimization finishes in a local minimum, but the optimization is slow.
#' When \code{'DE'} is used, \code{parini} should be specified as a list with
#' lower and upper bounds.
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{x, k, n}
#'   \item \code{groups} The grouping variables.
#'   \item \code{funname} String with the name of the shape of the curve.
#'   \item \code{psyfunguesslapses} Curve including guess and lapses.
#'   \item \code{limits} Limits of the curves.
#'   \item \code{parini} Initial parameters.
#'   \item \code{optimization} Method to optimize.
#'   \item \code{pariniset} \code{FALSE} if initial parameters are not given.
#'   \item \code{ypred} Predicted probabilities at the values of the explanatory
#'   variable.
#'   \item \code{curves} Curves.
#'   \item \code{par} Fitted parameters and its confidence intervals.
#'   \item \code{parcomnparisons} Pair-wise comparisons of the parameters
#'   to assess whether two parameters are significantly different
#'   using bootstrap. Specifically, the parameter bootstrap samples for each of
#'   the two conditions are substrated and then it is considered whether zero
#'   was within the confidence interval level of the distributions of differences.
#'   \item \code{curvesbootstrap} Bootstrap curves.
#'   \item \code{thresholds} Thresholds and its confidence intervals.
#'   \item \code{thresholdscomparisons} Pair-wise comparisons of the thresholds.
#'   \item \code{logliks} Log-likelihoods of the model.
#'   \item \code{loglikssaturated} Log-likelihoods of the saturated model.
#'   \item \code{deviance} Deviance of the model and the p-value calculated by
#'    bootstraping.
#'   \item \code{aic} AIC of the model defined as \deqn{ - 2 * loglik + 2  *k}
#'   where k is the number of parameters of the model.
#' }
#' @references
#' Burnham, K. P., & Anderson, D. R. (2003). Model selection and multimodel
#' inference: a practical information-theoretic approach. Springer Science &
#' Business Media.
#'
#' Knoblauch, K., & Maloney, L. T. (2012). Modeling Psychophysical Data in R.
#' New York: Springer.
#'
#' Prins, N., & Kingdom, F. A. A. (2016). Psychophysics: a practical
#' introduction. London: Academic Press.
#' @seealso \code{\link{quickpsy_}}
#' @examples
#' # make sure that all the requires packages are installed
#' # and loaded; instructions at https://github.com/danilinares/quickpsy
#' library(MPDiR) # contains the Vernier data; use ?Vernier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 10)
#' plot(fit)
#' plotpar(fit)
#' plotthresholds(fit, geom = 'point')

#' @export
#' @import MPDiR
#' @importFrom  graphics par
#' @importFrom stats approx as.formula lm median optim pnorm pweibull qnorm
#' quantile qweibull rbinom
#' @importFrom utils combn head read.table tail

quickpsy <- function(d, x = x, k = k, n = n,
                     grouping,
                     xmin = NULL, xmax = NULL,
                     log = FALSE,
                     fun = cum_normal_fun,
                     parini = NULL,
                     guess = 0, lapses = 0,
                     prob = NULL, thresholds = TRUE,
                     bootstrap = "parametric", B = 100, ci = .95) {

  x <- enquo(x)

  k <- enquo(k)

  if (!missing(n)) n <- enquo(n)
  else n <- NULL

  if (!missing(grouping)) {
    groups <- syms(as.character(enexpr(grouping))[-1])
  }
  else {
    d <- d %>%
      ungroup() %>%
      mutate(dummy_group = "g")
    groups <- syms("dummy_group")
  }



  # funname <- quo_name(enquo(fun))
  # if (funname %in% names(get_functions())) {
  #   cat(paste("Fitting the following function: ", funname, "\n"))
  # }

  if (missing(B) & bootstrap != "none")
    cat(paste("Using only", B, "bootstrap samples. Use the B argument, to modify it.\n"))

  if (!is.null(prob)) thresholds <- TRUE

  if (is.null(parini)) pariniset <- FALSE
  else pariniset <- TRUE

  if (is.logical(guess) && !guess) guess <- 0
  if (is.logical(lapses) && !lapses) lapses <- 0

  ### Calling functions
  nll_fun <- 5
  param <- 5
  ypred <- 5
  x_seq <- 5
  curves <- 5
  threshold <- 5

  averages <- averages(d, x, k, n, groups, log)

  limits <- limits(averages, x, xmin, xmax)

  psych_fun <- psych_fun(fun, guess, lapses)

  nll_fun <- nll_fun(averages, psych_fun, x)

  parini <- parini(averages, parini, psych_fun)

  param <- param(nll_fun, parini)

  ypred <- ypred(averages, param, psych_fun, x, log)

  x_seq <- x_seq(limits)

  curves <- ypred(x_seq, param, psych_fun, x, log)

  #
  # thresholds <- thresholds(param, curves, psych_fun, prob, log, guess, lapses)

   # conditions <- averages %>% distinct(UQS(groups(averages)))
   # conditions_conjoint <- fun %>% select(-fun)
   #
   # conditions_conjoint_names <- fun %>%
   #   select(-fun) %>%
   #   colnames()
   #
   # fun <- fun %>%
   #   group_by_at(vars(conditions_conjoint_names))


   #funname_df <- funname_df(conditions, funname)

  # fun_df <- fun_df(conditions, fun)
  #
  #  psyfunguesslapses_df <- psyfunguesslapses_df(fun_df, guess, lapses)


  #parini <- parini(averages, funname_df, x, guess, lapses, pariniset, parini)



  #
  # curves <- curves(par, limits, log, psyfunguesslapses)
  #
  # sse <-  sse(averages, ypred)
  #
  # if (thresholds) thresholds <-  thresholds(par, curves, prob, log, funname,
  #                                           guess, lapses)
  #
  # logliks <- logliks(averages, par, x, psyfunguesslapses)
  #
  # loglikssaturated <-  loglikssaturated(averages, par,
  #                                       x, k, n, psyfunguesslapses)
  #
  # deviance <- deviance(logliks, loglikssaturated)

  if (log) {
    averages <- averages %>% mutate(!!quo_name(x) := exp(!!x))
    curves$x <- exp(curves$x)
  }

  qp <- list(averages = averages,
             limits = limits,
             psych_fun = psych_fun,
             nll_fun = nll_fun,
             parini = parini,
             par = param,
             ypred = ypred,
             x_seq = x_seq,
             curves = curves,
             thresholds = thresholds)
#                funname_df = funname_df,
#                fun_df = fun_df,
#                psyfunguesslapses_df =psyfunguesslapses_df,
#                limits = limits,
#                parini = parini)
             # par = par,
             # ypred = ypred,
             # curves = curves,
             # sse = sse,
             # thresholds = thresholds,
             # logliks = logliks,
             # loglikssaturated = loglikssaturated,
             # deviance = deviance)

  return(qp)

  # qp <- c(qp, list(pariniset = pariniset))
  #

  #

  ##### llevar el tema de los log a fitpsy
  #if (log) qp$averages[[x]] <- exp(qp$averages[[x]])
  # if (log) {
  #   name_x <- quo_name(x)
  #   qp$averages <- qp$averages %>% mutate(!!name_x := log(!!x))
  # }

  ### bootstrap
 # if (bootstrap == "parametric" || bootstrap == "nonparametric") {
   # a <- 0
     #qp <- c(qp, list(avbootstrap = avbootstrap(qp, bootstrap, B)))

     #qp <- c(qp, list(parbootstrap = parbootstrap(qp)))
    # parci <- parci(qp, ci)
    # qp$par <- full_join(qp$par, parci, by= c('parn',qp$groups))
    #
    #
    # qp <- c(qp, list(logliksboot = logliksboot(qp)))
    # qp <- c(qp, list(logliksbootsaturated = logliksbootsaturated(qp)))
    # qp <- c(qp, list(devianceboot = devianceboot(qp)))
    # deviancep <- deviancep(qp)
    # qp$deviance <- merge(qp$deviance, deviancep)
    #
    # qp <- c(qp, list(aic = aic(qp)))
    #
    #
    # if (!(
    #   (length(qp$groups)==0) ||
    #   (length(qp$groups)==1 && nrow(unique(qp$averages[qp$groups]))==1)
    # )) {
    #   qp <- c(qp, list(parcomparisons = parcomparisons(qp, ci)))
    # }
    #
    # qp <- c(qp, list(curvesbootstrap = curvesbootstrap(qp, log = log)))
    #
    # if (thresholds) {
    #   qp <- c(qp,
    #           list(thresholdsbootstrap = thresholdsbootstrap(qp, prob, log)))
    #   thresholdsci = thresholdsci(qp, ci)
    #   qp$thresholds <- merge(qp$thresholds, thresholdsci)
    #   if (!(
    #     (length(qp$groups)==0) ||
    #     (length(qp$groups)==1 && nrow(unique(qp$averages[qp$groups]))==1)
    #   )) {
    #     qp <- c(qp, list(thresholdcomparisons = thresholdcomparisons(qp, ci)))
    #   }
    # }
  #}
  # else if (bootstrap != 'none')
  #   stop('Bootstrap should be \'parametric\', \'nonparametric\' or \'none\'.',
  #        call. = FALSE)


  # class(qp) <- "quickpsy"
  # qp
}

#' Data set for demonstration
#'
#' It is part of the data associated with the paper 'Motion signal and
#' the perceived positions of moving objects'.
#' @name qpdat
#' @docType data
#' @references Linares, D., L??pez-Moliner, J., & Johnston, A. (2007). Motion
#'signal and the perceived positions of moving objects. Journal of Vision,
#' 7(7), 1.

'qpdat'


