#' Run stock assessment model (with optional parallel processing)
#'
#' This function runs a stock assessment model using the specified iteration range.
#' Supports parallel execution on multiple CPU cores for faster computation.
#'
#' @param iter_range A numeric vector specifying the range of iterations to run the stock assessment model for (default is 4:100).
#' @param sim_data_path A character string specifying the path to the folder containing the simulation data files (default is the current working directory).
#' @param output_dir Character, the directory where simulation results will be saved.(default is the current working directory).
#' @param parameters A list containing the custom initial values for the parameters (default is NULL).
#' @param parameters.L A list containing the custom lower bounds for the parameters (default is NULL).
#' @param parameters.U A list containing the custom upper bounds for the parameters (default is NULL).
#' @param map A list containing the custom values for the map elements (default is NULL).
#' @param M Numeric, natural mortality (default: 0.2)
#' @param ncores Integer. Number of CPU cores to use. 1 = sequential (default).
#'   On Mac/Linux uses forked processes (mclapply); on Windows uses socket cluster (parLapply).
#'   Use \code{parallel::detectCores()} to see available cores.
#'
#' @return A list containing the results of the stock assessment model.
#' @export
sim_acl <- function(iter_range = 4:100, sim_data_path = ".", output_dir = ".",
                    parameters = NULL, parameters.L = NULL, parameters.U = NULL,
                    map = NULL, M = 0.2, ncores = 1) {

  # Compile DLL first (once, in main process)
  acl_info <- compile_and_load_acl()
  acl_dll_path <- acl_info$dll_path
  unload_acl(acl_dll_path)

  # Pre-compute shared parameter objects
  custom_bounds_and_params <- create_parameters(parameters, parameters.L, parameters.U)
  map_fixed <- generate_map(map)

  # --- Worker function: fits one iteration ---
  .fit_one_iter <- function(iter) {
    # Load simulation data
    sim.data <- NULL
    load(file.path(sim_data_path, paste0("sim_rep", iter)))

    na_matrix <- t(matrix(1, nrow = nrow(sim.data$SN_at_len), ncol = ncol(sim.data$SN_at_len)))
    na_matrix[which(sim.data$SN_at_len[, ] == 0)] <- 0

    tmb.data <- list(
      logN_at_len = t(log(sim.data$SN_at_len)),
      log_q       = log(sim.data$q_surv),
      len_border  = (sim.data$len_mid + 1)[1:(sim.data$nlen - 1)],
      na_matrix   = na_matrix,
      age         = sim.data$ages,
      Y           = sim.data$nyear,
      A           = sim.data$nage,
      L           = sim.data$nlen,
      weight      = sim.data$weight,
      mat         = sim.data$mat,
      M           = M
    )

    # Set up parameters (fresh copy for each worker)
    params_local <- custom_bounds_and_params$parameters
    params_local$dev_log_R  <- rep(0, sim.data$nyear)
    params_local$dev_log_F  <- array(0, c(sim.data$nage, sim.data$nyear))
    params_local$dev_log_N0 <- rep(0, (sim.data$nage - 1))

    lower <- unlist(custom_bounds_and_params$parameters.L)
    upper <- unlist(custom_bounds_and_params$parameters.U)

    rnames <- c("dev_log_R", "dev_log_F", "dev_log_N0")

    # Load DLL in this worker
    if (!is.loaded("ACL")) {
      dyn.load(acl_dll_path)
    }

    tryCatch({
      t_iter <- proc.time()
      obj <- TMB::MakeADFun(tmb.data, params_local, random = rnames, map = map_fixed,
                            DLL = "ACL", inner.control = list(trace = FALSE, maxit = 500),
                            silent = TRUE)

      opt <- nlminb(obj$par, obj$fn, obj$gr, lower = lower, upper = upper,
                    control = list(trace = 0, iter.max = 2000, eval.max = 10000))

      report <- obj$report()
      bound_check <- c((as.vector(opt$par) - as.vector(lower)),
                       (as.vector(upper) - as.vector(opt$par)))
      bound_hit <- min(bound_check) == 0

      sdresult <- TMB::sdreport(obj)
      est_std <- summary(sdresult)

      t_iter_elapsed <- (proc.time() - t_iter)[["elapsed"]]

      year <- 1:sim.data$nyear
      result <- list(obj = obj, opt = opt, report = report, est_std = est_std,
                     len_mid = sim.data$len_mid, year = year,
                     bound_hit = bound_hit, bound_check = bound_check,
                     converge = opt$message)

      # Save to disk
      save(result, file = file.path(output_dir, paste0("result_rep_", iter)))

      cat(sprintf("  iter %d: %s | boundary: %s | %.1f sec\n", iter, opt$message, bound_hit, t_iter_elapsed))
      return(result)

    }, error = function(e) {
      cat(sprintf("  iter %d: ERROR - %s\n", iter, conditionMessage(e)))
      return(list(converge = "FAILED", error = conditionMessage(e), iter = iter))
    }, finally = {
      if (is.loaded("ACL")) {
        dyn.unload(acl_dll_path)
      }
    })
  }

  # --- Dispatch: sequential vs parallel ---
  ncores <- min(ncores, length(iter_range))
  t_total <- proc.time()

  if (ncores <= 1) {
    # Sequential
    cat(sprintf("Running %d iterations sequentially...\n", length(iter_range)))
    result_list <- lapply(iter_range, .fit_one_iter)

  } else {
    cat(sprintf("Running %d iterations on %d cores", length(iter_range), ncores))

    if (.Platform$OS.type == "unix") {
      # macOS / Linux: fork-based (mclapply)
      cat(" [fork: mclapply]...\n")
      result_list <- parallel::mclapply(iter_range, .fit_one_iter, mc.cores = ncores)

    } else {
      # Windows: socket-based (parLapply)
      cat(" [socket: parLapply]...\n")
      cl <- parallel::makeCluster(ncores)

      # Export everything workers need
      parallel::clusterExport(cl, varlist = c(
        "sim_data_path", "output_dir", "M", "acl_dll_path",
        "custom_bounds_and_params", "map_fixed",
        ".fit_one_iter"
      ), envir = environment())

      # Load packages on each worker
      parallel::clusterEvalQ(cl, {
        library(TMB)
        library(ACL)
      })

      result_list <- parallel::parLapply(cl, iter_range, .fit_one_iter)
      parallel::stopCluster(cl)
    }
  }

  t_total_elapsed <- (proc.time() - t_total)[["elapsed"]]

  # Name the results
  names(result_list) <- paste0("result_rep_", iter_range)

  n_success <- sum(sapply(result_list, function(x) !identical(x$converge, "FAILED")))
  cat(sprintf("\n=== sim_acl complete: %d/%d succeeded | %.1f sec (%.1f min) ===\n",
              n_success, length(iter_range), t_total_elapsed, t_total_elapsed / 60))

  return(result_list)
}
