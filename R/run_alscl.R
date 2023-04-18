#' Run stock assessment model for user's data
#'
#' This function runs a stock assessment model using the specified iteration range.
#'
#' @param data.CatL A matrix containing the length grouping of the catch length.
#' @param data.wgt A matrix containing the length grouping of the weight.
#' @param data.mat A matrix containing the length grouping of the maturity.
#' @param rec.age Numeric, the age of recruitment.
#' @param nage Numeric, number of age classes.
#' @param len_mid Numeric vector, the midpoints of length bins.
#' @param len_border Numeric vector, the border of length bins.
#' @param mat_L50 Numeric, length at which 50% of individuals are mature.
#' @param mat_L95 Numeric, length at which 95% of individuals are mature.
#' @param parameters A list containing the custom initial values for the parameters (default is NULL).
#' @param parameters.L A list containing the custom lower bounds for the parameters (default is NULL).
#' @param parameters.U A list containing the custom upper bounds for the parameters (default is NULL).
#' @param map A list containing the custom values for the map elements (default is NULL).
#' @param M Numeric, natural mortality (default: 0.2)
#'
#' @return A list containing the results of the stock assessment model.
#' @export
run_alscl <- function(data.CatL,data.wgt,data.mat,rec.age,nage,M,len_mid,len_border,mat_L50,mat_L95,
                      parameters = NULL, parameters.L = NULL, parameters.U = NULL,
                      map = NULL) {

  library(TMB)



  #compile(file = "ACL.cpp", "&>/tmp/logfile.log")


  results_list <- list()

  nyear <- ncol(data.CatL)-1

  ages<-c(rec.age:(rec.age+nage-1))

  nlen<-as.numeric(nrow(data.CatL))


  log_q<-log(mat_func(mat_L50,mat_L95,len_mid))


  weight=data.wgt[,2:ncol(data.CatL)]
  mat=data.mat[,2:ncol(data.CatL)]

  # 检查 data.CatL 中是否存在 0 值
  if (any(data.CatL == 0)) {

    acl_cpp_path <- system.file("extdata", "ACL0.cpp", package = "ALSCL")

    if (acl_cpp_path == "") {
      stop("ACL0.cpp not found in the package directory.")
    }

    # 编译ACL.cpp
    compile(file = acl_cpp_path, "&> /tmp/logfile.log")

    # 创建一个新矩阵 na_matrix
    na_matrix <- matrix(1, nrow = nrow(data.CatL), ncol = ncol(data.CatL)-1)

    # 将 data.CatL 中的 0 值替换为新矩阵 na_matrix 中的相应值
    na_matrix[which(data.CatL[,2:ncol(data.CatL)]==0)]=0

    logN_at_len <- as.matrix(log(data.CatL[, 2:ncol(data.CatL)] + 1e-5))

    # 准备 tmb.data
    tmb.data <- list(
      logN_at_len = logN_at_len,
      na_matrix = na_matrix,
      log_q = log_q,
      len_border = len_border,
      age = ages,
      Y = nyear,
      A = nage,
      L = nlen,
      weight = as.matrix(weight),
      mat = as.matrix(mat),
      M = M
    )
    #
    custom_bounds_and_params <- create_parameters(parameters, parameters.L, parameters.U)
    parameters <- custom_bounds_and_params$parameters

    # random effects
    parameters$ dev_log_R = rep(0,nyear)
    parameters$ dev_log_F = array(0,c(nage,nyear))
    parameters$ dev_log_N0 = rep(0,(nage-1))

    parameters.L <- custom_bounds_and_params$parameters.L
    parameters.U <- custom_bounds_and_params$parameters.U


    lower=unlist(parameters.L)
    upper=unlist(parameters.U)

    map <- generate_map(map)
    rnames=c("dev_log_R","dev_log_F","dev_log_N0")

    #
    acl_cpp_dir <- dirname(acl_cpp_path)

    #
    acl_dll_path <- file.path(acl_cpp_dir, "ACL0.dll")

    #
    dyn.load(acl_dll_path)

    #dyn.load("ACL0")
    obj<-MakeADFun(tmb.data,parameters,random=rnames,map=map,DLL="ACL0",inner.control=list(trace=F, maxit=500))
    opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,control=list(trace=0,iter.max=2000,eval.max=10000))
    # opt1<-nlminb(opt$par,obj$fn,obj$gr,lower=lower,upper=upper,control=list(trace=0,iter.max=2000,eval.max=10000))
    # obj$gr(opt$par)
    # cbind(opt$par,lower,upper)
    report<-obj$report()

    result <- list(obj = obj, opt = opt, report = report,onverge = opt$message)

    #dyn.unload("ACL0")

    dyn.unload(acl_dll_path)
    results_list<-result
    rm(obj, opt, report, result)

  }


  else {

    acl_cpp_path <- system.file("extdata", "ACL.cpp", package = "ALSCL")

    if (acl_cpp_path == "") {
      stop("ACL.cpp not found in the package directory.")
    }

    # 编译ACL.cpp
    compile(file = acl_cpp_path, "&> /tmp/logfile.log")

    logN_at_len <- as.matrix(log(data.CatL[, 2:ncol(data.CatL)] + 1e-5))

    tmb.data=list(
      logN_at_len = t(logN_at_len),
      log_q = log_q,
      len_border = (len_mid + 1)[1:(nlen-1)],
      age = ages,
      Y = nyear,
      A = nage,
      L = nlen,
      weight = as.matrix(weight),
      mat=as.matrix(mat),
      M=M
    )



    #
    custom_bounds_and_params <- create_parameters(parameters, parameters.L, parameters.U)
    parameters <- custom_bounds_and_params$parameters

    # random effects
    parameters$ dev_log_R = rep(0,nyear)
    parameters$ dev_log_F = array(0,c(nage,nyear))
    parameters$ dev_log_N0 = rep(0,(nage-1))

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
    results_list<-result

    rm(obj, opt, report, result)

}
  return(results_list)
}
