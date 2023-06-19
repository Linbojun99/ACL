#' Plot Catch number over the years
#'
#' This function takes a list and a data frame as input, and plots CN over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "CN" component representing CN
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se_color Character. The color of the confidence interval ribbon. Default is "blue".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' plot_CN(model_result, line_size = 1.2, line_color = "red", line_type = "solid")
#' }
plot_CN <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2){
  # Extract the CN data
  CN <- model_result[["report"]][["CN"]]

  # Make sure it's a data frame
  if(!is.data.frame(CN)){
    CN <- as.data.frame(CN)
  }

  # Add Year to the CN data
  CN$Year <-  model_result[["year"]]

  if(!se)
  {
    # Plot CN over the years using ggplot2
    p <- ggplot2::ggplot(CN, aes(x = Year, y = CN)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::labs(x = "Year", y = "CN", title = "CN Over Years") +
      ggplot2::theme_minimal()
  }


  else
  {
    # Filter rows that contain "B"
    ss_CN <- model_result[["est_std"]][grep("^CN", rownames(model_result[["est_std"]])),]

    # Calculate confidence intervals
    confidence_intervals_CN <- data.frame(
      estimate = ss_CN[, "Estimate"],
      lower = ss_CN[, "Estimate"] - 1.96 * ss_CN[, "Std. Error"],
      upper = ss_CN[, "Estimate"] + 1.96 * ss_CN[, "Std. Error"]
    )

    confidence_intervals_CN$Year <-  model_result[["year"]]

    # Plot recruitment over the years with confidence intervals using ggplot2
    p <- ggplot2::ggplot(confidence_intervals_CN, aes(x = Year, y = estimate)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
      ggplot2::labs(y = "CN", x = "Year", title = "CN Over Years with Confidence Intervals") +
      ggplot2::theme_minimal()
  }

  return(p)
}

