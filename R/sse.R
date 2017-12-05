#' Sum of squared errors of prediction
#'
#' \code{ypred} calculates the sum of squared errors of prediction
#' @param qp output from quickpsy
#' @export
sse <- function(qp) {
  one_sse <- function(averages, ypred) {
    tibble(sse = sum((averages$prob-ypred$y)^2))
  }

  apply_to_two_elements(qp,
                        averages, ypred,
                        ~one_sse(.x, .y))

}
