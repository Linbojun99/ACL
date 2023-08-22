#' @title Run Age-Length Structured Stock Assessment Model for User's Data
#'
#' @description This function implements an age-length structured (ACL) model to
#' assess the status of a fish population given user's data. It aims to determine
#' the health of a fish stock and guide the sustainable management of fishery resources.
#'
#' @param data.CatL A matrix containing the length grouping of the catch length,
#' which represents the observed catch data in different length groups across years.
#' @param data.wgt A matrix containing the length grouping of the weight,
#' which indicates the average weight of fish in different length groups across years.
#' @param data.mat A matrix containing the length grouping of the maturity,
#' which represents the proportion of mature individuals in different length groups across years.
#' @param rec.age Numeric, the age at which individuals are assumed to be recruited to the fishery.
#' @param nage Numeric, the number of age classes considered in the model.
#' @param sel_L50 Numeric, the length at which 50% of individuals are mature,
#' representing the midpoint of the logistic selectivity curve.
#' @param sel_L95 Numeric, the length at which 95% of individuals are mature,
#' representing the point near the maximum of the logistic selectivity curve.
#' @param parameters A list containing the custom initial values for the parameters (default is NULL),
#' which can be used to fine-tune the model fitting.
#' @param parameters.L A list containing the custom lower bounds for the parameters (default is NULL),
#' which can be used to constrain the parameter search space during model fitting.
#' @param parameters.U A list containing the custom upper bounds for the parameters (default is NULL),
#' which can also be used to constrain the parameter search space during model fitting.
#' @param map A list containing the custom values for the map elements (default is NULL),
#' which may help in defining the structure of random effects.
#' @param M Numeric, the natural mortality rate,
#' which is an important input to the stock assessment model.
#' @param len_mid Numeric vector, user-specified median length values (default is NULL),
#' which can help in defining the length intervals for model fitting.
#' @param len_border Numeric vector, user-specified border length values (default is NULL),
#' which can also help in defining the length intervals for model fitting.
#' @param output Logical, If True, output the results of the model run in plots, tables, etc.(default is FALSE),
#' @param train_times Numeric, the number of times the model is to be trained,
#' with a default value of 1, corresponding to running the optimization routine once.
#' The user can specify a different number of training times to refine the model fit.
#'
#' @return A list containing the results of the ACL stock assessment model,
#' including model outputs (estimated parameters and their standard errors),
#' model fit diagnostics, and some other auxiliary information.
#' @export

run_acl <- function(data.CatL,data.wgt,data.mat,rec.age,nage,M,sel_L50,sel_L95,
                    parameters = NULL, parameters.L = NULL, parameters.U = NULL,
                    map = NULL,len_mid = NULL, len_border = NULL,output=FALSE,train_times=1)
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

    # Check if there is only one value
    contains_only_one_number <- function(s) {
      return(!grepl("-", s))
    }

    # If there is only one value, use len_border
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
    acl_cpp_path <- system.file("extdata", "ACL.cpp", package = "ACL")

    if (acl_cpp_path == "") {
      stop("ACL.cpp not found in the package directory.")
    }

    # Compile ACL.cpp
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
    acl_dll_path <- file.path(acl_cpp_dir, "ACL.dll")

    #
    dyn.load(acl_dll_path)

    #dyn.load("ACL")
    obj<-MakeADFun(tmb.data,parameters,random=rnames,map=map,DLL="ACL",inner.control=list(trace=F, maxit=500))

    cat("\nRunning optimization with nlminb...\n")

    opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,control=list(trace=0,iter.max=2000,eval.max=10000))

     for(i in 2:train_times) {
      opt<-nlminb(opt$par,obj$fn,obj$gr,lower=lower,upper=upper,control=list(trace=0,iter.max=2000,eval.max=10000))
    }
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

    len_label=data.CatL[,1]



    result <- list(obj = obj, opt = opt, report = report, est_std=est_std, year=year, len_mid=len_mid,len_label=len_label   ,bound_hit = bound_hit, bound_check = bound_check, converge = opt$message,final_outer_mgc=final_outer_mgc,par_low_up=par_low_up)

    #dyn.unload("ACL")

    dyn.unload(acl_dll_path)
    results_list<-result

    rm(obj, opt, report, result)



  # Check output parameters
if(output==TRUE){
  # Create 'figures' and 'tables' folders
  if (!dir.exists("output/figures/result")) {
    dir.create("output/figures/result")
    if (!dir.exists("output/figures/result")) {
      stop("Failed to create figures directory.")
    }
  }


  if (!dir.exists("output/figures/diagnostic")) {
    dir.create("output/figures/diagnostic")
    if (!dir.exists("output/figures/diagnostic")) {
      stop("Failed to create figures directory.")
    }
  }

  if (!dir.exists("output/tables")) {
    dir.create("output/tables")
    if (!dir.exists("output/tables")) {
      stop("Failed to create tables directory.")
    }
  }


    # Save the image in the output folder
    #png(filename="output/plot_abundance_N.png",width = 16, height = 9, units = "in", res = 600)
    plot_abundance(model_result=results_list, type = "N", line_size = 1.2, line_color = "red", se=T,line_type = "solid")
    ggsave(filename="output/figures/result/plot_abundance_N.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_abundance(model_result=results_list, type = "NA", line_size = 1.2, line_color = "red", line_type = "solid",se=T,facet_ncol = NULL)
    ggsave(filename="output/figures/result/plot_abundance_NA.png",width = 16, height = 9, units = "in", dpi = 600)


    plot_abundance(model_result=results_list, type = "NL", line_size = 1.2, line_color = "red", line_type = "solid",se=T,facet_ncol = NULL)
    ggsave(filename="output/figures/result/plot_abundance_NL.png",width = 16, height = 9, units = "in", dpi = 600)



    plot_biomass(model_result=results_list, type = "B", line_size = 1.2, line_color = "red", line_type = "solid",se=T,facet_ncol = NULL)
    ggsave(filename="output/figures/result/plot_biomass_B.png",width = 16, height = 9, units = "in", dpi = 600)


    plot_biomass(model_result=results_list, type = "BL", line_size = 1.2, line_color = "red", line_type = "solid",facet_ncol = NULL)
    ggsave(filename="output/figures/result/plot_biomass_BL.png",width = 16, height = 9, units = "in", dpi = 600)


    plot_catch(model_result=results_list, type = "CN", line_size = 1.2, line_color = "red", line_type = "solid",se=T,facet_ncol = NULL)
    ggsave(filename="output/figures/result/plot_catch_CN.png",width = 16, height = 9, units = "in", dpi = 600)


    plot_catch(model_result=results_list, type = "CNA", line_size = 1.2, line_color = "red", line_type = "solid",se=T,facet_ncol = NULL)
    ggsave(filename="output/figures/result/plot_catch_CNA.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_CatL(model_result=results_list,type = "length")
    ggsave(filename="output/figures/result/plot_CatL_length.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_CatL(model_result=results_list,type = "year",exp_transform = T)
    ggsave(filename="output/figures/result/plot_CatL_Year(exp=T).png",width = 16, height = 9, units = "in", dpi = 600)

    plot_fishing_mortality(model_result=results_list, line_size = 1.2, line_color = "red", line_type = "solid",se=T,type="year")
    ggsave(filename="output/figures/result/plot_fishing_mortality_year.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_fishing_mortality(model_result=results_list, line_size = 1.2, line_color = "red", line_type = "solid",se=T,type="age")
    ggsave(filename="output/figures/result/plot_fishing_mortality_age.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_pla(model_result=results_list)
    ggsave(filename="output/figures/result/plot_pla.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_recruitment(model_result=results_list, line_size = 1.2, line_color = "red", line_type = "solid",se=T)
    ggsave(filename="output/figures/result/plot_recruitment.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_ridges(model_result=results_list)
    ggsave(filename="output/figures/result/plot_ridges.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_SSB_Rec(model_result=results_list)
    ggsave(filename="output/figures/result/plot_SSB_Rec.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_SSB(model_result=results_list,type="SSB", line_size = 1.2, line_color = "red", line_type = "solid",se=T,facet_ncol = NULL)
    ggsave(filename="output/figures/result/plot_SSB.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_SSB(model_result=results_list,type="SBL", line_size = 1.2, line_color = "red", line_type = "solid",se=T,facet_ncol = NULL)
    ggsave(filename="output/figures/result/plot_SBL.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_VB(model_result=results_list, line_size = 1.2, line_color = "red", line_type = "solid",se=T)
    ggsave(filename="output/figures/result/plot_VB.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_residuals(model_result=results_list,type="length")
    ggsave(filename="output/figures/diagnostic/plot_residuals_length.png",width = 16, height = 9, units = "in", dpi = 600)

    plot_residuals(model_result=results_list,type="year")
    ggsave(filename="output/figures/diagnostic/plot_residuals_year.png",width = 16, height = 9, units = "in", dpi = 600)


    #####table output

    diagnostics<-diagnose_model(data.CatL=data.CatL,model_result=results_list)
    write.csv(diagnostics, file = "output/tables/diagnostics.csv",row.names = F)


    aN=plot_abundance(model_result=results_list, type = "N",se=T,return_data = T)
    write.csv(aN[["data"]], file = "output/tables/abundance_N.csv",row.names = F)

    aNA=plot_abundance(model_result=results_list, type = "NA", se=T,return_data = T)
    write.csv(aNA[["data"]], file = "output/tables/abundance_NA.csv",row.names = F)

    aNL=plot_abundance(model_result=results_list, type = "NL", se=T,return_data = T)
    write.csv(aNL[["data"]], file = "output/tables/abundance_NL.csv",row.names = F)

    bB=plot_biomass(model_result=results_list, type = "B", se=T,return_data = T)
    write.csv(bB[["data"]], file = "output/tables/biomass_B.csv",row.names = F)

    bBL=plot_biomass(model_result=results_list, type = "BL", se=T,return_data = T)
    write.csv(bBL[["data"]], file = "output/tables/biomass_BL.csv",row.names = F)

    cCN=plot_catch(model_result=results_list, type = "CN", se=T,return_data = T)
    write.csv(cCN[["data"]], file = "output/tables/biomass_CN.csv",row.names = F)

    cCNA=plot_catch(model_result=results_list, type = "CNA", se=T,return_data = T)
    write.csv(cCNA[["data"]], file = "output/tables/biomass_CNA.csv",row.names = F)

    fmy=plot_fishing_mortality(model_result=results_list, type = "year",se=T,return_data = T)
    write.csv(fmy[["data"]], file = "output/tables/fishing_mortality_year.csv",row.names = F)

    fma=plot_fishing_mortality(model_result=results_list, type = "age",se=T,return_data = T)
    write.csv(fma[["data"]], file = "output/tables/fishing_mortality_age.csv",row.names = F)

    r=plot_recruitment(model_result=results_list, se=T,return_data = T)
    write.csv(r[["data"]], file = "output/tables/recruitment.csv",row.names = F)

    sr=plot_SSB_Rec(model_result=results_list,return_data = T)
    write.csv(sr[["data"]], file = "output/tables/SSB_Rec.csv",row.names = F)

    catll=plot_CatL(model_result=results_list,type = "length",return_data = T)
    write.csv(catll[["data1"]], file = "output/tables/CatL_length_Estimated.csv",row.names = F)
    write.csv(catll[["data2"]], file = "output/tables/CatL_length_Observed.csv",row.names = F)

    catll=plot_CatL(model_result=results_list,type = "year",exp_transform = T,return_data = T)
    write.csv(catll[["data1"]], file = "output/tables/CatL_year_Estimated(exp=T).csv",row.names = F)
    write.csv(catll[["data2"]], file = "output/tables/CatL_year_Observed(exp=T).csv",row.names = F)

    plot_CatL(model_result=results_list,type = "year",exp_transform = T)
    ggsave(filename="output/figures/result/plot_CatL_Year(exp=T).png",width = 16, height = 9, units = "in", dpi = 600)


    ssb=plot_SSB(model_result=results_list, type = "SSB",se=T,return_data = T)
    write.csv(ssb[["data"]], file = "output/tables/SSB.csv",row.names = F)

    sbl=plot_SSB(model_result=results_list, type = "SBL",se=T,return_data = T)
    write.csv(sbl[["data"]], file = "output/tables/SBL.csv",row.names = F)

    rl<-plot_residuals(model_result=results_list,type="length",return_data = T)
    write.csv(rl[["data"]], file = "output/tables/residuals_length.csv",row.names = F)

    ry<-plot_residuals(model_result=results_list,type="year",return_data = T)
    write.csv(ry[["data"]], file = "output/tables/residuals_year.csv",row.names = F)

    }
    return(results_list)
    }

}
