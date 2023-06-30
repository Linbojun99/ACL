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
#' @param M Numeric, natural mortality
#' @param len_mid Numeric vector, user-specified median length values (default is NULL).
#' @param len_border Numeric vector, user-specified border length values (default is NULL).
#'
#' @return A list containing the results of the stock assessment model.
#' @export
run_acl <- function(data.CatL,data.wgt,data.mat,rec.age,nage,M,sel_L50,sel_L95,
                    parameters = NULL, parameters.L = NULL, parameters.U = NULL,
                    map = NULL,len_mid = NULL, len_border = NULL)
{
  {

    library(TMB)



    #compile(file = "ACL.cpp", "&>/tmp/logfile.log")


    results_list <- list()

    nyear <- ncol(data.CatL)-1

    ages<-c(rec.age:(rec.age+nage-1))

    nlen<-as.numeric(nrow(data.CatL))

    na_matrix<-matrix(1,nrow=nrow(data.CatL),ncol=ncol(data.CatL)-1)

    na_matrix[which(data.CatL[,2:ncol(data.CatL)]==0)]=0


    weight=data.wgt[,2:ncol(data.CatL)]
    mat=data.mat[,2:ncol(data.CatL)]

    # 检查是否只有一个数值
    contains_only_one_number <- function(s) {
      return(!grepl("-", s))
    }

    # 如果只有一个数值，使用 len_border
    if (contains_only_one_number(data.CatL[,1][1])) {
      len_mid <- len_border
      log_q <- log(mat_func(sel_L50, sel_L95, len_mid))
    } else {

    if (is.null(len_mid)) {

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
    }
    else {
      len_mid <- len_mid
    }

    log_q<-log(mat_func(sel_L50,sel_L95,len_mid))
}
    if (is.null(len_border)) {
      extract_last_number <- function(range_str) {
        parts <- strsplit(range_str, "-")[[1]]
        last_number <- as.numeric(parts[2])
        return(last_number)
      }


      len_border <- sapply(data.CatL[-nrow(data.CatL),1], extract_last_number)

    } else {
      len_border <- len_border
    }
    acl_cpp_path <- system.file("extdata", "ACL0.cpp", package = "ALSCL")

    if (acl_cpp_path == "") {
      stop("ACL.cpp not found in the package directory.")
    }

    # 编译ACL.cpp
    compile(file = acl_cpp_path, "&> /tmp/logfile.log")

    logN_at_len <- as.matrix(log(data.CatL[, 2:ncol(data.CatL)]+1e-5 ))

    tmb.data=list(
      logN_at_len = logN_at_len,
      na_matrix=na_matrix,
      log_q = log_q,
      len_border =len_border,
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
    acl_dll_path <- file.path(acl_cpp_dir, "ACL0.dll")

    #
    dyn.load(acl_dll_path)

    #dyn.load("ACL")
    obj<-MakeADFun(tmb.data,parameters,random=rnames,map=map,DLL="ACL0",inner.control=list(trace=F, maxit=500))

    cat("\nRunning optimization with nlminb...\n")

    opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,control=list(trace=0,iter.max=2000,eval.max=10000))
    # opt1<-nlminb(opt$par,obj$fn,obj$gr,lower=lower,upper=upper,control=list(trace=0,iter.max=2000,eval.max=10000))
    final_outer_mgc<-obj$gr(opt$par)
    par_low_up<- cbind(opt$par,lower,upper)
    report<-obj$report()
    bound_check<-c((as.vector(opt$par)-as.vector(lower)),(as.vector(upper)-as.vector(opt$par)))
    bound_hit<-min(bound_check)==0

    cat("\nRunning sdreport...\n")

    sdresult<-sdreport(obj)
    est_std<-summary(sdresult)


    cl_l <- tidyr::gather(data.CatL,key="Year",value="length",2:ncol(data.CatL))
    year <- cl_l %>%
      dplyr::mutate(Year = as.numeric(gsub("X", "", Year))) %>%
      dplyr::distinct(Year) %>%
      dplyr::pull(Year)



    result <- list(obj = obj, opt = opt, report = report, est_std=est_std, year=year, len_mid=len_mid,   bound_hit = bound_hit, bound_check = bound_check, converge = opt$message,final_outer_mgc=final_outer_mgc,par_low_up=par_low_up)

    #dyn.unload("ACL")

    dyn.unload(acl_dll_path)
    results_list<-result

    rm(obj, opt, report, result)

  }
  return(results_list)
}
