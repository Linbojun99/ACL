#' Plot Recruitment over the years
#'
#' This function visualizes the recruitment of a population over a period of time.
#' Recruitment is a key parameter in population dynamics, representing the amount of new individuals added to a population,
#' either by birth or immigration, over a specific time frame. The resulting plot is a line graph, with the option to add a
#' ribbon that illustrates the standard error, if the `se` argument is set to TRUE.
#'
#' @param model_result A list that contains the model output. The list should have a "report" component which contains a "Rec" component representing Recruitment.
#' @param line_size Numeric. Specifies the thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. Specifies the color of the line in the plot. Default is "red".
#' @param line_type Character. Specifies the type of the line in the plot. Default is "solid".
#' @param se Logical. Determines whether to calculate and plot the standard error as confidence intervals. Default is FALSE.
#' @param se_color Character. Specifies the color of the confidence interval ribbon. Default is "red".
#' @param se_alpha Numeric. Specifies the transparency of the confidence interval ribbon. Default is 0.2.
#'
#' @return A ggplot object representing the plot.
#'
#' @examples
#' \dontrun{
#' # Use 'run_acl' to obtain 'model_result'
#' model_result <- run_acl(...)
#'
#' # Plot recruitment without standard error
#' plot_recruitment(model_result)
#'
#' # Plot recruitment with standard error
#' plot_recruitment(model_result, se = TRUE)
#' }
#'
#' @export

plot_recruitment <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2){
  # Extract the recruitment data
  recruitment <- model_result[["report"]][["Rec"]]

  # Make sure it's a data frame
  if(!is.data.frame(recruitment)){
    recruitment <- as.data.frame(recruitment)
  }

  # Add Year to the recruitment data
  recruitment$Year <- model_result[["year"]]
  if (!se){
    # Plot recruitment over the years using ggplot2
    p <- ggplot2::ggplot(recruitment, aes(x = Year, y = recruitment)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::labs(x = "Year", y = "Recruitment", title = "Recruitment Over Years") +
      ggplot2::theme_minimal()
  } else {

     # Filter rows that contain "Rec"
    ss_rec <- model_result[["est_std"]][grep("Rec", rownames(model_result[["est_std"]])),]

    # Calculate confidence intervals
    confidence_intervals_rec <- data.frame(
      estimate = ss_rec[, "Estimate"],
      lower = ss_rec[, "Estimate"] - 1.96 * ss_rec[, "Std. Error"],
      upper = ss_rec[, "Estimate"] + 1.96 * ss_rec[, "Std. Error"]
    )

    confidence_intervals_rec$Year <-  model_result[["year"]]

    # Plot recruitment over the years with confidence intervals using ggplot2
    p <- ggplot2::ggplot(confidence_intervals_rec, aes(x = Year, y = estimate)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
      ggplot2::labs(y = "Recruitment", x = "Year", title = "Recruitment Over Years with Confidence Intervals") +
      ggplot2::theme_minimal()
  }
  return(p)
}
