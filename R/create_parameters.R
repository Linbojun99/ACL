#' Create custom parameter bounds for the stock assessment model
#'
#' This function generates custom lower and upper bounds for the parameters of the stock assessment model.
#' Users can provide custom values for specific parameters, and the function will use the default values for the others.
#'
#' @param parameters A list containing the custom initial values for the parameters (default is NULL).
#'   - log_init_Z: initial log(Z) value
#'   - log_std_log_N0: standard deviation of log(N0)
#'   - mean_log_R: mean of log(R)
#'   - log_std_log_R: standard deviation of log(R)
#'   - logit_log_R: logit of log(R)
#'   - mean_log_F: mean of log(F)
#'   - log_std_log_F: standard deviation of log(F)
#'   - logit_log_F_y: logit of log(F_y)
#'   - logit_log_F_a: logit of log(F_a)
#'   - log_vbk: log(vBk)
#'   - log_Linf: log(Linf)
#'   - log_t0: log(t0)
#'   - log_cv_len: log(cv_len)
#'   - log_std_index: log(std_index)
#' @param parameters.L A list containing the custom lower bounds for the parameters (default is NULL).
#' @param parameters.U A list containing the custom upper bounds for the parameters (default is NULL).
#'
#' @return A list containing three elements:'parameters' 'parameters.L' and 'parameters.U', representing the generated lower and upper bounds for the parameters.
#' @export
create_parameters <- function(parameters = NULL,parameters.L = NULL, parameters.U = NULL) {
  default_parameters <- list(
    log_init_Z = 0.5,
    log_std_log_N0 = log(0.5),

    mean_log_R = 5,
    log_std_log_R = log(0.2),
    logit_log_R = log(0.75/0.25),

    mean_log_F = log(0.3),
    log_std_log_F = log(0.8),
    logit_log_F_y = log(0.75/0.25),
    logit_log_F_a = log(0.75/0.25),

    log_vbk = log(0.2),
    log_Linf = log(60),
    log_t0 = log(1/60),
    log_cv_len = log(0.3),

    log_std_index = log(0.1)
  )



default_parameters.L = list(
    log_init_Z = log(0.01),
    log_std_log_N0 = -Inf,
    mean_log_R = log(10),
    log_std_log_R = log(0.01),
    logit_log_R = -30,
    mean_log_F = log(0.01),
    #log_std_log_F = log(0.01),
    #logit_log_F_y = -10,
    #logit_log_F_a = -10,
    log_vbk = log(0.1),
    log_Linf = log(10),
    #log_t0 = -20,
    log_cv_len = log(0.01),
    log_std_index = -20
  )

  default_parameters.U = list(
    log_init_Z = log(1),
    log_std_log_N0 = log(1),
    mean_log_R = 10,
    log_std_log_R = log(1),
    logit_log_R = 20,
    mean_log_F = log(1),
    #log_std_log_F = log(1),
    #logit_log_F_y = 20,
    #logit_log_F_a = 10,
    log_vbk = log(1),
    log_Linf = log(100),
    #log_t0 = 0,
    log_cv_len = log(1),
    log_std_index = log(1)
  )

  if (!is.null(parameters)) {
    updated_parameters <- modifyList(default_parameters, parameters)
  } else {
    updated_parameters <- default_parameters
  }

  if (!is.null(parameters.L)) {
    updated_parameters.L <- modifyList(default_parameters.L, parameters.L)
  } else {
    updated_parameters.L <- default_parameters.L
  }

  if (!is.null(parameters.U)) {
    updated_parameters.U <- modifyList(default_parameters.U, parameters.U)
  } else {
    updated_parameters.U <- default_parameters.U
  }

  # Filter the outputs based on provided parameters
  if (!is.null(parameters)) {
    updated_parameters <- updated_parameters[names(parameters)]
  }
  if (!is.null(parameters.L)) {
    updated_parameters.L <- updated_parameters.L[names(parameters.L)]
  }
  if (!is.null(parameters.U)) {
    updated_parameters.U <- updated_parameters.U[names(parameters.U)]
  }

  return(list(parameters = updated_parameters, parameters.L = updated_parameters.L, parameters.U = updated_parameters.U))
}
