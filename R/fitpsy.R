#' Fits the curve
#' \code{fitpsy} fits de curve
#' @keywords internal
#' @export fitpsy
fitpsy <- function(d, x, k, n, grouping, xmin, xmax, log, fun, funname, parini,
                   pariniset, guess, lapses, optimization) {

  d <- d %>% rename(k = !!k, n = !!n)

  if (is.logical(guess) && !guess) guess <- 0
  if (is.logical(lapses) && !lapses) lapses <- 0

  groups <- c()
  if (!missing(grouping)) groups <- c(groups, grouping)
  groups_x <- c(groups, quo_name(x))

  #grouping <- syms(grouping)
  # if (is.null(n)) {
  #   d[[k]][d[[k]] == -1] <- as.numeric(0)
  #   d <- d %>% group_by_(.dots=c(groups, x)) %>%
  #     summarise_(n = 'n()', k = paste0('sum(',k,')'))
  #   names(d)[names(d) == 'k'] <- k
  #   n <- 'n'
  # }

  d <- d  %>% ungroup()

  if (is.null(n)) {
    d <- d %>% group_by(!!!syms(groups_x)) %>%
      summarise(n = n(), k = sum(!!k))
  }

  if (!missing(grouping)) d <- d %>% group_by(!!!syms(groups))

  # prob <- NULL
  # d$prob <- d[[k]] / d[[n]]
  #if (log) d[[x]] <- log(d[[x]])



  d <- d %>% mutate(prob = k / n)


  name_x <- quo_name(x)
  if (log) d <- d %>% mutate(!!name_x := log(!!x))

  psyfunguesslapses <- create_psy_fun(fun, guess, lapses)

  limits <- limits(d, x, xmin, xmax)

  #groups <- as.character(groups(d))


  if (!pariniset)
    if (funname %in% names(get_functions())) {
      parini <- parini(d, x, guess, lapses, funname)
      cat(paste("Fitting the following function:", funname, "\n"))
    }
    else stop('parini (initial parameters) must be specified.')


  par <- parameters(d, x, k, n, psyfunguesslapses, funname,
                     parini, pariniset, guess, lapses, optimization, groups)


  list(x = x, k = k , n = n, guess = guess, lapses = lapses,
       averages = d,
       groups = groups, funname = funname, log = log,
       psyfunguesslapses = psyfunguesslapses, limits = limits,
       pariniset = pariniset, parini = parini,
       optimization = optimization, par = par)
}


