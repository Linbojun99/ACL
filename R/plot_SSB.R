#' Plot Spawning stock biomass over the years
#'
#' This function takes a list and a data frame as input, and plots SSB over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "SSB" component representing SSB
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
#' plot_SSB(model_result, line_size = 1.2, line_color = "red", line_type = "solid")
#' }
plot_SSB <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2){
  # Extract the SSB data
  SSB <- model_result[["report"]][["SSB"]]

  # Make sure it's a data frame
  if(!is.data.frame(SSB)){
    SSB <- as.data.frame(SSB)
  }

  # Add Year to the SSB data
  SSB$Year <-  model_result[["year"]]

  if(!se)
  {
    # Plot SSB over the years using ggplot2
    p <- ggplot2::ggplot(SSB, aes(x = Year, y = SSB)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::labs(x = "Year", y = "SSB", title = "SSB Over Years") +
      ggplot2::theme_minimal()
  }


  else
  {
    # Filter rows that contain "B"
    ss_ssb <- model_result[["est_std"]][grep("^SSB", rownames(model_result[["est_std"]])),]

    # Calculate confidence intervals
    confidence_intervals_ssb <- data.frame(
      estimate = ss_ssb[, "Estimate"],
      lower = ss_ssb[, "Estimate"] - 1.96 * ss_ssb[, "Std. Error"],
      upper = ss_ssb[, "Estimate"] + 1.96 * ss_ssb[, "Std. Error"]
    )

    confidence_intervals_ssb$Year <-  model_result[["year"]]

    # Plot recruitment over the years with confidence intervals using ggplot2
    p <- ggplot2::ggplot(confidence_intervals_ssb, aes(x = Year, y = estimate)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
      ggplot2::labs(y = "SSB", x = "Year", title = "SSB Over Years with Confidence Intervals") +
      ggplot2::theme_minimal()
  }

  return(p)
}

