#' Run stock assessment model
#'
#' This function runs a stock assessment model using the specified iteration range.
#'
#' @param iter_range A numeric vector specifying the range of iterations to run the stock assessment model for (default is 4:100).
#' @param sim_data_path A character string specifying the path to the folder containing the simulation data files (default is the current working directory).
#' @param output_dir Character, the directory where simulation results will be saved.(default is the current working directory).
#' @param parameters A list containing the custom initial values for the parameters (default is NULL).
#' @param parameters.L A list containing the custom lower bounds for the parameters (default is NULL).
#' @param parameters.U A list containing the custom upper bounds for the parameters (default is NULL).
#' @param map A list containing the custom values for the map elements (default is NULL).
#' @param M Numeric, natural mortality (default: 0.2)
#'
#' @return A list containing the results of the stock assessment model.
#' @export
sim_acl <- function(iter_range = 4:100,sim_data_path = ".",output_dir=".", parameters = NULL, parameters.L = NULL, parameters.U = NULL,map = NULL,M=0.2) {

  library(TMB)

  acl_cpp_path <- system.file("extdata", "ACL.cpp", package = "ALSCL")

  if (acl_cpp_path == "") {
    stop("ACL.cpp not found in the package directory.")
  }

  # 编译ACL.cpp
  compile(file = acl_cpp_path, "&> /tmp/logfile.log")


  #compile(file = "ACL.cpp", "&>/tmp/logfile.log")


  results_list <- list()

  for(iter in iter_range) {
    load(file.path(sim_data_path, paste0("sim_rep", iter)))


    tmb.data=list(
      logN_at_len = t(log(sim.data$SN_at_len)),
      log_q = log(sim.data$q_surv),
      len_border = (sim.data$len_mid + 1)[1:(sim.data$nlen-1)],
      age = sim.data$ages,
      Y = sim.data$nyear,
      A = sim.data$nage,
      L = sim.data$nlen,
      weight = sim.data$weight,
      mat=sim.data$mat,
      M=M
    )



    #
    custom_bounds_and_params <- create_parameters(parameters, parameters.L, parameters.U)
    parameters <- custom_bounds_and_params$parameters

    # random effects
    parameters$ dev_log_R = rep(0,sim.data$nyear)
    parameters$ dev_log_F = array(0,c(sim.data$nage,sim.data$nyear))
    parameters$ dev_log_N0 = rep(0,(sim.data$nage-1))

    parameters.L <- custom_bounds_and_params$parameters.L
    parameters.U <- custom_bounds_and_params$parameters.U


    lower=unlist(parameters.L)
    upper=unlist(parameters.U)

    map <- generate_map(map)
    rnames=c("dev_log_R","dev_log_F","dev_log_N0")

    #
    acl_cpp_dir <- dirname(acl_cpp_path)

    #
    acl_dll_path <- file.path(acl_cpp_dir, "ACL.dll")

    #
    dyn.load(acl_dll_path)

    #dyn.load("ACL")
    obj<-MakeADFun(tmb.data,parameters,random=rnames,map=map,DLL="ACL",inner.control=list(trace=F, maxit=500))
    opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,control=list(trace=0,iter.max=2000,eval.max=10000))
    # opt1<-nlminb(opt$par,obj$fn,obj$gr,lower=lower,upper=upper,control=list(trace=0,iter.max=2000,eval.max=10000))
    # obj$gr(opt$par)
    # cbind(opt$par,lower,upper)
    report<-obj$report()
    bound_check<-c((as.vector(opt$par)-as.vector(lower)),(as.vector(upper)-as.vector(opt$par)))
    bound_hit<-min(bound_check)==0

    result <- list(obj = obj, opt = opt, report = report, bound_hit = bound_hit, bound_check = bound_check, converge = opt$message)

    #dyn.unload("ACL")

    dyn.unload(acl_dll_path)
    save(result, file = file.path(output_dir, paste0("result_rep_", iter)))

    results_list[[paste0("result_rep_", iter)]] <- result
    rm(obj, opt, report, result)
  }

  return(results_list)
}
