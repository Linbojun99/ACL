#' @title Maturation function
#' @description Calculate maturation probability based on the length of the fish.
#' @param L50 Length at which 50% of individuals are mature.
#' @param L95 Length at which 95% of individuals are mature.
#' @param length Length of the fish.
#' @return Maturation probability.
#' @export
mat_func <- function(L50, L95, length) {
  b1 <- log(0.95 / 0.05) / (L95 - L50)
  bo <- -L50 * b1
  logit_pt <- bo + b1 * length
  matp <- exp(logit_pt) / (1 + exp(logit_pt))
  return(matp)
}
