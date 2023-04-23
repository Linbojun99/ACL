#' Run stock assessment model for user's data
#'
#' This function runs a stock assessment model using the specified iteration range.
#'
#' @param data.CatL A matrix containing the length grouping of the catch length.
#' @param data.wgt A matrix containing the length grouping of the weight.
#' @param data.mat A matrix containing the length grouping of the maturity.
#' @param rec.age Numeric, the age of recruitment.
#' @param nage Numeric, number of age classes.
#' @param sel_L50 Numeric, length at which 50% of individuals are mature.
#' @param sel_L95 Numeric, length at which 95% of individuals are mature.
#' @param parameters A list containing the custom initial values for the parameters (default is NULL).
#' @param parameters.L A list containing the custom lower bounds for the parameters (default is NULL).
#' @param parameters.U A list containing the custom upper bounds for the parameters (default is NULL).
#' @param map A list containing the custom values for the map elements (default is NULL).
#' @param M Numeric, natural mortality (default: 0.2)
#'
#' @return A list containing the results of the stock assessment model.
#' @export
run_alscl1 <- function(data.CatL,data.wgt,data.mat,rec.age,nage,M,sel_L50,sel_L95,
                      parameters = NULL, parameters.L = NULL, parameters.U = NULL,
                      map = NULL)
  {
  {

  library(TMB)



  #compile(file = "ACL.cpp", "&>/tmp/logfile.log")


  results_list <- list()

  nyear <- ncol(data.CatL)-1

  ages<-c(rec.age:(rec.age+nage-1))

  nlen<-as.numeric(nrow(data.CatL))




  weight=data.wgt[,2:ncol(data.CatL)]
  mat=data.mat[,2:ncol(data.CatL)]


  contains_special_char <- function(s) {
    return(grepl("<|>", s))
  }


  range_to_bin <- function(range_str) {
    parts <- strsplit(range_str, "-")[[1]]
    first_number <- as.numeric(parts[1])
    last_number <- as.numeric(parts[2])
    bin <- (last_number - first_number) / 2
    return(bin)
  }

  bin <- range_to_bin(data.CatL[,1][2])

  range_to_median <- function(range_str, isFirst = FALSE, isLast = FALSE, prev_range_str = "", next_range_str = "") {
    if (contains_special_char(range_str)) {
      parts <- strsplit(range_str, "-")[[1]]
      single_number <- as.numeric(gsub("[^0-9]", "", parts[1]))

      if (isFirst) {
        next_parts <- strsplit(next_range_str, "-")[[1]]
        next_first_number <- as.numeric(next_parts[1])
        next_last_number <- as.numeric(next_parts[2])
        bin <- (next_last_number - next_first_number) / 2
        len_mid <- single_number + bin
      } else {
        prev_parts <- strsplit(prev_range_str, "-")[[1]]
        prev_first_number <- as.numeric(prev_parts[1])
        prev_last_number <- as.numeric(prev_parts[2])
        bin <- (prev_last_number - prev_first_number) / 2
        len_mid <- single_number - bin
      }
    } else {
      parts <- strsplit(range_str, "-")[[1]]
      first_number <- as.numeric(parts[1])
      last_number <- as.numeric(parts[2])
      len_mid <- last_number - bin
    }

    return(round(len_mid))
  }


  len_mid <- sapply(seq_along(data.CatL[,1]), function(i) {
    prev_range_str <- if (i > 1) data.CatL[,1][i - 1] else ""
    next_range_str <- if (i < length(data.CatL[,1])) data.CatL[,1][i + 1] else ""
    range_to_median(data.CatL[,1][i], isFirst = i == 1, isLast = i == length(data.CatL[,1]), prev_range_str = prev_range_str, next_range_str = next_range_str)
  })


  log_q<-log(mat_func(sel_L50,sel_L95,len_mid))


  extract_last_number <- function(range_str) {
    parts <- strsplit(range_str, "-")[[1]]
    last_number <- as.numeric(parts[2])
    return(last_number)
  }

  len_border <- sapply(data.CatL[,1], extract_last_number)


    acl_cpp_path <- system.file("extdata", "ACL.cpp", package = "ALSCL")

    if (acl_cpp_path == "") {
      stop("ACL.cpp not found in the package directory.")
    }

    # 编译ACL.cpp
    compile(file = acl_cpp_path, "&> /tmp/logfile.log")

    logN_at_len <- as.matrix(log(data.CatL[, 2:ncol(data.CatL)]+ 1e-2 ))

    tmb.data=list(
      logN_at_len = logN_at_len,
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
