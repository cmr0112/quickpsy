#' Print quickpsy objects
#'@export print.quickpsy
print.quickpsy <- function(x,...)
{
  print('Parameters')
  print(x$par)
  if ('thresholds' %in% names(x)) {
    print('Thresholds')
    print(x$thresholds)
  }
}
