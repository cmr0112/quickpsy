#' Sum of squared errors of prediction
#'
#' \code{sse} calculates the sum of squared errors of prediction
#' @export
sse <- function(averages, ypred) {
  one_sse <- function(averages, ypred) {
    tibble(sse = sum((averages$prob-ypred$y)^2))
  }

  apply_to_two_elements(averages, ypred, one_sse)

}
