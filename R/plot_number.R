#' Plot number over the years
#'
#' This function takes a list and a data frame as input, and plots number over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "N" component representing number.
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
#' plot_number(model_result, line_size = 1.2, line_color = "red", line_type = "solid")
#' }
plot_number <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2){
  # Extract the number data
  number <- model_result[["report"]][["N"]]

  # Make sure it's a data frame
  if(!is.data.frame(number)){
    number <- as.data.frame(number)
  }

  # Add Year to the number data
  number$Year <-  model_result[["year"]]

  if(!se)
  {
    # Plot number over the years using ggplot2
    p <- ggplot2::ggplot(number, aes(x = Year, y = number)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::labs(x = "Year", y = "number", title = "number Over Years") +
      ggplot2::theme_minimal()
  }


  else
  {
    # Filter rows that contain "N"
    ss_n <- model_result[["est_std"]][grepl("^N(?!A|L)", rownames(model_result[["est_std"]]), perl = TRUE),]

    # Calculate confidence intervals
    confidence_intervals_n <- data.frame(
      estimate = ss_n[, "Estimate"],
      lower = ss_n[, "Estimate"] - 1.96 * ss_n[, "Std. Error"],
      upper = ss_n[, "Estimate"] + 1.96 * ss_n[, "Std. Error"]
    )

    confidence_intervals_n$Year <-  model_result[["year"]]

    # Plot recruitment over the years with confidence intervals using ggplot2
    p <- ggplot2::ggplot(confidence_intervals_n, aes(x = Year, y = estimate)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
      ggplot2::labs(y = "Number", x = "Year", title = "Number Over Years with Confidence Intervals") +
      ggplot2::theme_minimal()
  }

  return(p)
}

