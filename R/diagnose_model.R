#' Diagnose model results
#'
#' This function receives a model output and performs various checks, including checking if the model has reached its boundaries, if it has converged, and printing the final likelihood estimate.
#'
#' @param data.CatL A matrix containing the length grouping of the catch length,
#' which represents the observed catch data in different length groups across years.
#' @param model_result A list that contains the model output. The list should have a "report" component which contains an "Elog_index" component representing counts in length groups.
#' @return NULL. The function prints out the diagnostics and does not return anything.
#' @export
#' @examples
#' \dontrun{
#' diagnose_model(data.CatL,model_result)
#' }
diagnose_model <- function(data.CatL,model_result) {
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
  if("converge" %in% names(model_result)) {
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

  observed_data <-data.CatL[,2:ncol(data.CatL)]
  Elog_index <- model_result[["report"]][["Elog_index"]]
  estimated_data<-exp(Elog_index)


  # Remove observations equal to 0, and remove the corresponding predicted values
  non_zero_indices <- observed_data != 0
  observed_data <- observed_data[non_zero_indices]
  estimated_data <- estimated_data[non_zero_indices]

  # Calculation error
  errors <- observed_data - estimated_data

  # Calculation MSE
  MSE <- mean(errors^2)

  # Calculation MAE
  MAE <- mean(abs(errors))

  # Calculation RMSE
  RMSE <- sqrt(MSE)

  # Calculate the sum of squared residuals
  sse <- sum(errors^2)

  # Calculate the total sum of squares
  sst <- sum((observed_data - mean(observed_data))^2)

  # Calculate R-squared
  Rsquared <- 1 - sse/sst

  # Calculate MAPE
  MAPE <- mean(abs((observed_data - estimated_data) / observed_data)) * 100

  # Calculate Explained Variance Score
  exp_var_score <- 1 - var(observed_data - estimated_data) / var(observed_data)

  # Calculate Max Error
  max_error <- max(abs(observed_data - estimated_data))


  bound_hit<-model_result[["bound_hit"]]
  converge<-model_result[["converge"]]
  final_outer_mgc<-model_result[["final_outer_mgc"]][length(model_result[["final_outer_mgc"]]) - 2]
  # Create a data frame with the results
  diagnostics <- data.frame(Metric = c("MSE", "MAE", "RMSE", "Rsquared", "MAPE", "Explained Variance Score", "Max Error",
                                       "Boundary Hit", "Model Converged", "Final Outer mgc"),
                            Value = c(MSE, MAE, RMSE, Rsquared, MAPE, exp_var_score, max_error,
                                      bound_hit, converge, final_outer_mgc))
  return(diagnostics)
}
