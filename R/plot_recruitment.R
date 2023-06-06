#' Plot Recruitment over the years
#'
#' This function takes a list and a data frame as input, and plots recruitment over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "F" component representing Recruitment.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.5.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @param se_color Character. The color of the confidence interval ribbon. Default is "blue".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' plot_recruitment(model_result, line_size = 1.2, line_color = "red", line_type = "solid")
#' }
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
