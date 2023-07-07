#' Diagnostic function for ACL
#'
#' @param data.CatL A matrix containing the length grouping of the catch length,
#' which represents the observed catch data in different length groups across years.
#' @param model_result A list that contains the model output. The list should have a "report" component which contains an "Elog_index" component representing counts in length groups.
#' @return A list containing the calculated metrics: MSE, MAE, RMSE, Rsquared, MAPE, exp_var_score, max_error
#' @export
#' @examples
#' diagnostic_metrics(data.CatL, model_result)

diagnostic_metrics <- function(data.CatL, model_result) {

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

  return(list(MSE = MSE, MAE = MAE, RMSE = RMSE, Rsquared = Rsquared, MAPE = MAPE, exp_var_score = exp_var_score, max_error = max_error))
}
