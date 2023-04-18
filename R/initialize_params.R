#' Initialize parameters for the fish simulation
#'
#' This function initializes the parameters and variables needed for the fish simulation.(default:)
#'
#' @param nyear Integer, number of years (default: 100)
#' @param rec.age Integer, the age of recruitment to fishery (default: 1)
#' @param first.year Integer, the first year of projection (default: 2020)
#' @param nage Integer, number of ages (default: 15)
#' @param M Numeric, natural mortality (default: 0.2)
#' @param init_Z Numeric, initial total mortality leading to equilibrium age structure (default: 0.5)
#' @param vbk Numeric, von Bertalanffy growth parameter (default: 0.2)
#' @param Linf Numeric, asymptotic length (default: 60)
#' @param t0 Numeric, growth parameter (default: 1/60)
#' @param len_mid Numeric vector, the midpoints of length bins.(default: seq(6,50,2))
#' @param len_border Numeric vector, the border values of length bins.(default: seq(5,51,2))
#' @param a parameter to define length–weight relationship.(default: exp(-12))
#' @param b parameter to define length–weight relationship.(default: 3)
#' @param mat_L50 Numeric, length at which 50% of individuals are mature.(default: 35)
#' @param mat_L95 Numeric, length at which 95% of individuals are mature.(default: 40)
#' @param cv_L Numeric, the coefficient of variance in length at age at the beginning year.(default: 0.2)
#' @param cv_inc Numeric, the coefficient of variance in growth increment.(default: 0.2)
#' @param std_logR Numeric, standard deviation of recruitment.(default: 0.3)
#' @param std_logN0 Numeric, standard deviation of initial number at age.(default: 0.2)
#' @param alpha Numeric, Beverton-Holt model parameter.(default: 400)
#' @param beta Numeric, Beverton-Holt model parameter.(default: 10)
#' @param std_SN Numeric, survey measurement error.(default: 0.2)
#' @param q_surv_L50 Numeric, survey catchability at length for 50% catchability.(default: 15)
#' @param q_surv_L95 Numeric, survey catchability at length for 95% catchability.(default: 20)
#'
#' @return A list containing the initialized parameters and variables
#' @export
initialize_params <- function(nyear = 100, rec.age = 1, first.year = 2020, nage = 15, M = 0.2,
                              init_Z = 0.5, vbk = 0.2, Linf = 60, t0 = 1/60 ,
                              len_mid = seq(6,50,2), len_border = seq(5,51,2),
                              a = exp(-12), b = 3,
                              mat_L50 = 35, mat_L95 = 40,
                              cv_L = 0.2,cv_inc = 0.2,
                              std_logR = 0.3,std_logN0 = 0.2,
                              alpha = 400,beta = 10,
                              std_SN = 0.2,q_surv_L50 = 15,q_surv_L95 = 20) {
  ages <- c(rec.age:(rec.age + nage - 1)) # list of ages in the simulation
  years <- c(first.year:(first.year + nyear - 1)) # list of years in the simulation

  ### biological variables

  #len_mid<-seq(6,50,2)
  #len_border<-seq(5,51,2)
  len_border[1]=-Inf
  len_border[length(len_border)]=Inf

  ### weigth at length
  #a = exp(-12); b = 3

  ### maturation at length
  #mat_L50 = 35; mat_L95 = 40

  #cv_L = 0.2  # the coefficient of variance in length at age at the beginning year
  #cv_inc = 0.2  # the coefficient of variance in growth increment

  #std_logR = 0.3  # standard deviation of recruitment
  #std_logN0 = 0.2  # standard deviation of initial number at age

  ### SR relationship (BH model) aS/(b+S)
  # alpha = 400
  # beta = 10

  #### survey variables
  #std_SN = 0.2  # survey measurement error
  #q_surv_L50 = 15  # survey catchability at length
  #q_surv_L95 = 20

  params <- list(nyear = nyear, rec.age = rec.age, first.year = first.year, nage = nage, M = M, init_Z = init_Z, vbk = vbk, Linf = Linf, t0 = t0, ages = ages, years = years,
                 len_mid=len_mid,len_border=len_border,a=a,b=b,mat_L50=mat_L50,mat_L95=mat_L95,
                 cv_L=cv_L,cv_inc=cv_inc,std_logR=std_logR,std_logN0=std_logN0,alpha=alpha,beta=beta,
                 std_SN=std_SN,q_surv_L50=q_surv_L50,q_surv_L95=q_surv_L95)

  return(params)
}
