#' Diagnose model results
#'
#' This function receives a model output and performs various checks, including checking if the model has reached its boundaries, if it has converged, and printing the final likelihood estimate.
#'
#' @param model_result The model output. This should be a list that contains "bound_hit", "converge", and "final_outer_mgc" components.
#' @return NULL. The function prints out the diagnostics and does not return anything.
#' @export
#' @examples
#' \dontrun{
#' diagnose_model(model_result)
#' }
diagnose_model <- function(model_result) {
  if(!is.list(model_result)) stop("Model output should be a list.")

  # Check if the model result has hit the boundaries
  if("bound_hit" %in% names(model_result)) {
    if(model_result[["bound_hit"]]==TRUE) {
      cat("\nWarning: The model result has hit the boundaries.\n")
    } else {
      cat("\nThe model result has not hit the boundaries.\n")
    }
  } else {
    cat("\nWarning: 'bound_hit' not found in the model output.\n")
  }

  # Check if the model has converged
  # Check model convergence
  if("converge" %in% names(model)) {
    convergence <- grepl("relative convergence", model_result[["converge"]])
    if(convergence==TRUE) {
      cat("\nThe model has converged.\n")
    } else {
      cat("\nWarning: The model has not converged.\n")
    }
  } else {
    cat("\nWarning: 'converge' not found in the model output.\n")
  }
  # Print the final likelihood estimate
  if("final_outer_mgc" %in% names(model_result)) {
    outer_mgc <- model_result[["final_outer_mgc"]][length(model_result[["final_outer_mgc"]]) - 2]
    cat("\nThe final_outer_mgc is ", outer_mgc, ".\n")
  } else {
    cat("\nWarning: 'final_outer_mgc' not found in the model output.\n")
  }

  return(NULL)
}
