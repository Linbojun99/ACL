#' YTF(yellowtail flounder) data list
#'
#' Parameter values are specified based on empirical data of yellowtail flounder on the grand Bank off Newfoundland
#'
#' @format A list with 24 elements:
#' \describe{
#'   \item{nyear}{Number of time steps used for model diagnostic.}
#'   \item{nlen}{Number of length bins.}
#'   \item{nage}{number of age classes.}
#'   \item{len_mid}{the midpoints of length bins.}
#'   \item{len_border}{the border values of length bins.}
#'   \item{len_lower}{lower bounds of length bins (cm).}
#'   \item{len_upper}{upper bounds of length bins (cm).}
#'   \item{vbk}{growth rate parameter.}
#'   \item{Linf}{asymptotic length parameter.}
#'   \item{t0}{growth parameter.}
#'   \item{rec.age}{age class of recruitment.}
#'   \item{init_Z}{initial mortality to determine age structure in initial time step.}
#'   \item{cv_L}{the coefficient of variance in length at age at the beginning year.}
#'   \item{cv_inc}{the coefficient of variance in growth increment.}
#'   \item{M}{natural mortality.}
#'   \item{std_logR}{standard deviation of recruitment.}
#'   \item{std_logN0}{standard deviation of initial number at age.}
#'   \item{std_SN}{survey measurement error.}
#'   \item{a}{parameter to define length–weight relationship.}
#'   \item{b}{parameter to define length–weight relationship.}
#'   \item{alpha}{Beverton-Holt model parameter.}
#'   \item{beta}{Beverton-Holt model parameter.}
#'   \item{mat_L50}{length at which 50% of individuals are mature.}
#'   \item{mat_L95}{length at which 95% of individuals are mature.}
#'   \item{q_surv_L50}{survey catchability at length for 50% catchability.}
#'   \item{q_surv_L95}{survey catchability at length for 95% catchability.}



#'
#' }
#' @source Parameter values are specified based on empirical data of yellowtail flounder on the grand Bank off Newfoundland .
#' @references Your references, if applicable.
YTF <- list(
  nyear = 20,
  nlen = length(seq(6,50,2)),
  nage=15,
  len_mid=seq(6,50,2),
  len_border=c(-Inf,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,Inf),
  len_lower = c(-Inf,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49),
  len_upper = c(7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,Inf),
  vbk  = 0.2,
  Linf  = 60,
  t0 = 1/60,
  rec.age=1,
  init_Z  = 0.5,
  cv_L = 0.2,
  cv_inc  = 0.2,
  M = 0.2,
  std_logR = 0.3,
  std_logN0 = 0.2,
  std_SN = 0.1,
  a = exp(-12),
  b = 3,
  alpha = 400,
  beta = 10,
  mat_L95  = 40,
  mat_L50  = 35,
  q_surv_L95  = 20,
  q_surv_L50  = 15
)
