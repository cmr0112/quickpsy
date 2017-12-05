#' L-BFGS-B optimization
#' @keywords internal
#' @export
lbfgsb <- function(d, parini,
                        x, k, n,
                        psyfunguesslapses, funname,
                        pariniset,
                        guess, lapses) {

  nllfun <- create_nll(d, x, psyfunguesslapses)

  parini <- matrix(unlist(parini), ncol = 2, byrow = TRUE)
  para <- optim(.5 * (parini[,1] + parini[,2]),
                nllfun,
                method = "L-BFGS-B",
                lower = parini[,1],
                upper = parini[,2])$par



  tibble(parn = paste0("p", seq(1, length(para))), par = para)
}
