#' Simulate Fishery Dynamics
#'
#' This function simulates fishery dynamics based on the given biological and fishing variables and initial state variables.
#'
#' @param bio_fishing_vars A list containing the biological and fishing variables.
#' @param init_state_vars A list containing the initial state variables.
#' @param nyear Integer, number of years to simulate.
#' @param output_dir Character, the directory where simulation results will be saved.(default is the current working directory).
#'
#' @return A list containing simulated fishery data.
#' @export
sim_data <- function(bio_fishing_vars, init_state_vars, nyear,output_dir=".") {
  nyear<-nyear
  # Extract biological and fishing variables from the input list
  len_lower <- bio_fishing_vars$len_lower
  len_upper <- bio_fishing_vars$len_upper
  nlen <- bio_fishing_vars$nlen
  LatA <- bio_fishing_vars$LatA
  W_at_len <- bio_fishing_vars$W_at_len
  mat <- bio_fishing_vars$mat
  pla <- bio_fishing_vars$pla
  q_surv <- bio_fishing_vars$q_surv
  sel <- bio_fishing_vars$sel

  # Extract initial state variables from the input list
  nage <- init_state_vars$nage
  ages <- init_state_vars$ages
  years <- init_state_vars$years
  vbk <- init_state_vars$vbk
  t0 <- init_state_vars$t0
  M <- init_state_vars$M
  Linf <- init_state_vars$Linf
  init_Z <- init_state_vars$init_Z
  t0 <- init_state_vars$t0
  len_mid <- init_state_vars$len_mid
  std_logR <- init_state_vars$std_logR
  std_logN0 <- init_state_vars$std_logN0
  std_SN <- init_state_vars$std_SN
  q_surv_L50 <- init_state_vars$q_surv_L50
  q_surv_L95 <- init_state_vars$q_surv_L95
  alpha <- init_state_vars$alpha
  beta <- init_state_vars$beta


  # Initialize arrays or data structures for storing simulated data
  # recruitment
  for(iter in 4:100){
  Rec<-rep(NA,nyear)
  R_init=500 # initial number of recruitment
  set.seed(iter)
  dev_logR = arima.sim(list(order=c(1,0,0), ar=0.1), n=nyear)*std_logR # std of rec is 0.2
  for(i in 1:nyear){
    Rec[i]<-exp(log(R_init)+dev_logR[i])
  }

  # mortality
  Z_at_age<-matrix(NA,nrow=nyear,ncol=nage) # total mortality at age
  F_at_age<-matrix(NA,nrow=nyear,ncol=nage) # fishing mortality at age
  M_at_age<-matrix(M,nrow=nyear,ncol=nage) # natural mortality at age
  set.seed(iter)
  F_yr<-0.3*exp(arima.sim(list(order=c(1,0,0), ar=0.75), n=nyear)*0.2) # annual fishing mortality




  # Run the simulation for nyear
  for (year in 1:nyear) {
    # recruitment
    Rec<-rep(NA,nyear)
    R_init=500 # initial number of recruitment
    set.seed(iter)
    dev_logR = arima.sim(list(order=c(1,0,0), ar=0.1), n=nyear)*std_logR # std of rec is 0.2
    for(i in 1:nyear){
      Rec[i]<-exp(log(R_init)+dev_logR[i])
    }

    # mortality
    Z_at_age<-matrix(NA,nrow=nyear,ncol=nage) # total mortality at age
    F_at_age<-matrix(NA,nrow=nyear,ncol=nage) # fishing mortality at age
    M_at_age<-matrix(M,nrow=nyear,ncol=nage) # natural mortality at age
    set.seed(iter)
    F_yr<-0.3*exp(arima.sim(list(order=c(1,0,0), ar=0.75), n=nyear)*0.2) # annual fishing mortality
    for(i in 1:nyear){
      F_at_age[i,]<-F_yr[i]*sel
      Z_at_age[i,]<-F_at_age[i,]+M_at_age[i,]
    }

    N_at_age<-matrix(NA,nrow=nyear,ncol=nage) # abundance at age
    N_at_len<-matrix(NA,nrow=nyear,ncol=nlen) # abundance at length

    # first year age structure
    set.seed(iter)
    N0_at_age = rep(NA,nage)
    dev_logN0 = rnorm((nage-1),0,std_logN0)
    N0_at_age[1]<-Rec[1]
    for(i in 2:nage){
      N0_at_age[i]=N0_at_age[i-1]*exp(-init_Z+dev_logN0[i-1])
    }

    # age-based cohort dynamics

    # define variables
    N_at_age<-matrix(NA,nrow=nyear,ncol=nage) # number at age
    N_at_len<-matrix(NA,nrow=nyear,ncol=nlen) # number at length
    #B_at_age<-matrix(NA,nrow=nyear,ncol=nage) # biomass at age
    B_at_len<-matrix(NA,nrow=nyear,ncol=nlen) # biomass at length
    #SB_at_age<-matrix(NA,nrow=nyear,ncol=nage) # spawning biomass at age
    SB_at_len<-matrix(NA,nrow=nyear,ncol=nlen) # spawning biomass at length
    CN_at_age<-matrix(NA,nrow=nyear,ncol=nage) # catch number at age
    CN_at_len<-matrix(NA,nrow=nyear,ncol=nlen) # catch number at length
    #CB_at_age<-matrix(NA,nrow=nyear,ncol=nage) # catch biomass at age
    CB_at_len<-matrix(NA,nrow=nyear,ncol=nlen) # catch biomass at length
    SSB=rep(NA,nyear)
    TN=rep(NA,nyear)
    TB=rep(NA,nyear)
    CN=rep(NA,nyear)
    CB=rep(NA,nyear)

    # Initialization
    N_at_age[1,]=N0_at_age
    N_at_len[1,]=N_at_age[1,]%*%pla
    B_at_len[1,]=N_at_len[1,]*W_at_len
    SB_at_len[1,]=B_at_len[1,]*mat
    SSB[1]=sum(SB_at_len[1,])

    # forward dynamics
    for(i in 2:nyear)
    {
      # recruitment process
      logR=log(alpha*SSB[i-1]/(beta + SSB[i-1])) # BH model
      Rec[i]<-exp(logR+dev_logR[i]) # add autocorrelated error
      N_at_age[i,1]<-Rec[i]

      for(j in 2:(nage-1))
      {
        # survival process
        N_at_age[i,j]<-N_at_age[i-1,j-1]*exp(-Z_at_age[i-1,j-1])
      }
      # plus group
      N_at_age[i,nage]<-N_at_age[i-1,nage-1]*exp(-Z_at_age[i-1,nage-1]) + N_at_age[i-1,nage]*exp(-Z_at_age[i-1,nage])

      N_at_len[i,]=N_at_age[i,]%*%pla
      B_at_len[i,]=N_at_len[i,]*W_at_len
      SB_at_len[i,]=B_at_len[i,]*mat
      SSB[i]=sum(SB_at_len[i,])

    }

    # derive other quantities
    for(i in 1:nyear)
    {
      for(j in 1:nage)
      {
        CN_at_age[i,j]=N_at_age[i,j]*(1-exp(-Z_at_age[i,j]))*(F_at_age[i,j]/Z_at_age[i,j])
      }
      CN_at_len[i,]=CN_at_age[i,]%*%pla
      CB_at_len[i,]=CN_at_len[i,]*W_at_len
      TN[i]=sum(N_at_len[i,])
      TB[i]=sum(B_at_len[i,])
      CN[i]=sum(CN_at_len[i,])
      CB[i]=sum(CB_at_len[i,])
    }

    # calculate research vessel survey index

    RVN_at_len=matrix(NA,nrow=nyear,ncol=nlen)
    RVB_at_len=matrix(NA,nrow=nyear,ncol=nlen)

    set.seed(iter)
    surv_error<-rnorm(nyear,0,std_SN)
    for(i in 1:nyear)
    {
      RVN_at_len[i,]=N_at_len[i,]*q_surv*exp(surv_error[i])
      RVB_at_len[i,]=RVN_at_len[i,]*W_at_len
    }

    for(i in 1:nyear)
    {
      for(j in 1:nlen)
      {
        if(RVN_at_len[i,j]<1e-6){RVN_at_len[i,j]=1e-6}
      }
    }

    RVN=rowSums(RVN_at_len)
    RVB=rowSums(RVB_at_len)


  }

  # Store the simulated fishery data in a list
  sim.data <- list(
    SN_at_len = RVN_at_len[81:100,],
    q_surv = q_surv,
    len_mid = len_mid,
    nyear=nyear-80,
    nage=nage,
    nlen=nlen,
    ages=c(1:nage),
    weight=matrix(rep(W_at_len,nyear-80),nrow=nlen,ncol=nyear-80),
    mat=matrix(rep(mat,nyear-80),nrow=nlen,ncol=nyear-80),

    sel = sel,
    F_at_age = F_at_age[81:100,],
    M_at_age = M_at_age[81:100,],
    N_at_len = N_at_len[81:100,],
    N_at_age = N_at_age[81:100,],
    B_at_len = B_at_len[81:100,],
    SB_at_len = SB_at_len[81:100,],
    CN_at_len = CN_at_len[81:100,],
    CN_at_age = CN_at_age[81:100,],
    CB_at_len = CB_at_len[81:100,],
    RVN_at_len = RVN_at_len[81:100,],
    RVB_at_len = RVB_at_len[81:100,],
    Rec = N_at_age[81:100,1],
    TN = TN[81:100],
    TB = TB[81:100],
    CN = CN[81:100],
    CB = CB[81:100],
    SSB = SSB[81:100]
  )

  # Save the simulated fishery data to a file in the specified output directory
  save(sim.data, file = file.path(output_dir, paste0("sim_rep", iter)))

  }
  return(sim.data)
}
