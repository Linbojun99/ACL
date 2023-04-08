#' @title Von Bertalanffy growth function
#' @description Calculate length at age using the von Bertalanffy growth function.
#' @param Linf The asymptotic length.
#' @param k The growth coefficient.
#' @param t0 The theoretical age at zero length.
#' @param age Age of the fish.
#' @return Length at age.
#' @export
VB_func <- function(Linf, k, t0, age) {
  Lt = Linf * (1 - exp(-k * (age - t0)))
  return(Lt)
}
