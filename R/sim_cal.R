#' Calculate biological and fishing variables
#'
#' This function calculates and returns biological and fishing variables for the simulation model.
#'
#' @param params A list containing the parameters variables.
#'
#' @return A list containing the biological and fishing variables.
#' @export
sim_cal <- function(params)
 {
  # Extract parameters from the input list(Initialize parameters for the fish simulation)
  nyear <- params$nyear
  nage <- params$nage
  ages <- params$ages
  rec.age<- params$rec.age
  M <- params$M
  init_Z <- params$init_Z
  vbk <- params$vbk
  Linf <- params$Linf
  t0 <- params$t0

  len_mid<-params$len_mid
  len_border<-params$len_border

  a=params$a
  b=params$b

  mat_L50=params$mat_L50
  mat_L95=params$mat_L95

  cv_L = params$cv_L  # the coefficient of variance in length at age at the beginning year
  cv_inc = params$cv_inc  # the coefficient of variance in growth increment

  std_logR = params$std_logR  # standard deviation of recruitment
  std_logN0 = params$std_logR  # standard deviation of initial number at age

  # SR relationship (BH model) aS/(b+S)
  alpha = params$alpha
  beta = params$beta


  # survey variables
  std_SN = params$std_SN  # survey measurement error
  q_surv_L50 = params$q_surv_L50  # survey catchability at length
  q_surv_L95 = params$q_surv_L95


  # Calculate biological and fishing variables
  nlen<-length(len_mid)
  len_lower=len_border[1:nlen]
  len_upper=len_border[2:(nlen+1)]

  ages<-c(rec.age:(rec.age+nage-1))


  LatA <- VB_func(Linf, vbk, t0, ages)

  # weigth at length

  W_at_len = a * (len_mid ^ b)

  # maturation at length
  mat = mat_func(mat_L50, mat_L95, len_mid)



  # age-length transition matrix
  pla = matrix(NA, nrow = nage, ncol = nlen)  # transfer age to length
  ml = VB_func(Linf, vbk, t0, ages)
  sl = cv_L * ml
  for (i in 1:nlen) {
    pla[, i] = pnorm(len_border[i + 1], ml, sl) - pnorm(len_border[i], ml, sl)
  }

  # survey variables
  q_surv = mat_func(q_surv_L50, q_surv_L95, len_mid)

  # fishing variable
  sel = rep(1, nage)

  # Store the results in a list
  # Store the results in a list
  calculated_bio_vars <- list(
    LatA = LatA,
    W_at_len = W_at_len,
    mat = mat,
    pla = pla,
    q_surv = q_surv,
    sel = sel,
    len_lower=len_lower,
    len_upper=len_upper,
    nlen=nlen
  )


  return(calculated_bio_vars)
}
