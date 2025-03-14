#' @title Plot the Relationship between Spawning Stock Biomass (SSB) and Recruitment (Rec)
#'
#' @description This function takes the model result as an input and creates a scatter plot to visualize the relationship between
#' Spawning Stock Biomass (SSB) and Recruitment (Rec). These metrics are fundamental in the context of fishery stock assessment.
#'
#' @param model_result A list containing the model results. It includes a component "report" that comprises "SSB" and "Rec".
#' @param point_color Character. The color of the points in the scatter plot. Default is "black".
#' @param point_size Numeric. The size of the points in the scatter plot. Default is 2.
#' @param point_shape Numeric. The shape of the points in the scatter plot, as an integer value (see ?points in base R for more info). Default is 16 (filled circle).
#' @param return_data A logical indicating whether to return the processed data alongside the plot. Default is FALSE.
#'
#' @return A ggplot object representing the scatter plot of SSB versus Recruitment.
#'
#' @examples
#' \dontrun{
#' # The model_result is typically obtained from the run_acl() function. Here, we create a dummy example.
#' # Use 'run_acl' to get 'model_result'
#' model_result <- run_acl(...)
#'
#' # Call the function
#' p <- plot_SSB_Rec(model_result)
#'
#' # Print the plot
#' print(p)
#' }
#' @export
plot_SSB_Rec <- function(model_result, age_at_recruitment = 1,
                         point_size=2,point_color="black",point_shape=16, return_data = FALSE) {


  # Extract the necessary data
  SSB_data <- model_result[["report"]][["SSB"]]
  Rec_data <- model_result[["report"]][["Rec"]]

  # Ensure age_at_recruitment does not exceed data length
  if (age_at_recruitment >= length(SSB_data)) {
    stop("age_at_recruitment is too large compared to data length.")
  }
  
  # Adjust SSB to align with Rec based on recruitment age
  SSB_adjusted <- SSB_data[1:(length(SSB_data) - age_at_recruitment)]
  Rec_adjusted <- Rec_data[(age_at_recruitment + 1):length(Rec_data)]
  
  # Create a data frame for ggplot
  plot_data <- data.frame(SSB = SSB_adjusted, Rec = Rec_adjusted)

  # Create the plot
  p <-  ggplot2::ggplot(plot_data, aes(x = SSB, y = Rec)) +
    ggplot2::geom_point(size= point_size, color= point_color,shape=point_shape) +
    ggplot2:: labs(x = "SSB", y = "Rec")+
    ggplot2::theme_minimal()

  data_out <-plot_data

  if (return_data) {
    return(list(plot = p, data = data_out))
  } else {
    return(p)
  }
}




