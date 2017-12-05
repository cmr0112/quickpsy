#' Fits the curve
#' \code{fitpsy} fits de curve
#' @keywords internal
#' @export fitpsy
fitpsy <- function(d, x, k, n,
                   grouping,
                   xmin, xmax,
                   log,
                   fun, funname,
                   parini, pariniset,
                   guess, lapses) {


  if (is.logical(guess) && !guess) guess <- 0
  if (is.logical(lapses) && !lapses) lapses <- 0

  groups <- c()
  if (!missing(grouping)) groups <- c(groups, grouping)
  groups_x <- c(groups, quo_name(x))

  d <- d  %>% ungroup()

  if (is.null(n)) {
    d <- d %>% rename(k = !!k)
    averages <- d %>% group_by(!!!syms(groups_x)) %>%
      summarise(n = n(), k = sum(k))
  }
  else {
    averages <- d %>% rename(k = !!k, n = !!n)
  }

  if (!missing(grouping)) averages <- averages %>% group_by(!!!syms(groups))

  averages <- averages %>% mutate(prob = k / n)

  name_x <- quo_name(x)

  if (log) averages <- averages %>% mutate(!!name_x := log(!!x))

  psyfunguesslapses <- create_psy_fun(fun, guess, lapses)

  limits <- limits(averages, x, xmin, xmax, groups)

  #groups <- as.character(groups(d))

  if (!pariniset)
    if (funname %in% names(get_functions())) {
      parini <- parini(averages, x, guess, lapses, funname, groups)
      cat(paste("Fitting the following function: ", funname, "\n"))
    }
    else {
      stop("parini (initial parameters) must be specified.")
    }


  par <- parameters(averages, parini,
                    x, k, n,
                    psyfunguesslapses, funname,
                    pariniset, guess, lapses,
                    groups)



  list(x = x, k = k , n = n,
       guess = guess, lapses = lapses,
       log = log,
       funname = funname,
       averages = averages,
       psyfunguesslapses = psyfunguesslapses,
       pariniset = pariniset, parini = parini,
       limits = limits,
       par = par,
       groups = groups)
}


