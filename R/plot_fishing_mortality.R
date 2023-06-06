#' Plot fishing mortality over the years
#'
#' This function takes a matrix and a data frame as input, and plots fishing mortality over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "BL" component representing counts in length groups.
#' @param line_size Line size for geom_line. Default is 1.
#' @param line_color Line color for geom_line. Default is "black".
#' @param line_type Line type for geom_line. Default is "solid".
#' @param facet_ncol Number of columns in facet wrap. Default is 3.
#' @param facet_scales Scales for facet wrap. Default is "free".
#' @param se_color Character. The color of the confidence interval ribbon. Default is "blue".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' plot_fishing_mortality(F, data.cl)
#' }
plot_fishing_mortality <- function(model_result, line_size = 1, line_color = "red", line_type = "solid", facet_ncol = 3, facet_scales = "free" ,se = FALSE, se_color = "red", se_alpha = 0.2){

  # Extract the F data
  F<- model_result[["report"]][["F"]]

  # Create Year variable from column names of F (assuming columns are years)
  Year <- model_result[["year"]]

  # Create AgeGroup variable from row names of F
  AgeGroup <- paste0("Age group ", seq_len(nrow(F)))

  # Convert matrix to data frame in long format
  F_long <- reshape2::melt(F)
  colnames(F_long) <- c("AgeGroup", "Year", "F")
  F_long$Year <- Year[as.numeric(F_long$Year)]
  F_long$AgeGroup <- AgeGroup[as.numeric(F_long$AgeGroup)]
if(!se)
  {
  # Plot fishing mortality over the years using ggplot2
  p <- ggplot2::ggplot(F_long, aes(x = Year, y = F)) +
    ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
    ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
    ggplot2::labs(x = "Year", y = "Fishing Mortality", title = "Fishing Mortality Over Years") +
    ggplot2::theme_minimal()

}

else
  {

  # Filter rows that contain "F"
  ss_f <- model_result[["est_std"]][grep("^F", rownames(model_result[["est_std"]])),]
  ss_f<-as.data.frame(ss_f)
  # Calculate confidence intervals
  confidence_intervals_f <- data.frame(
    estimate = ss_f[, "Estimate"],
    lower = ss_f[, "Estimate"] - 1.96 * ss_f[, "Std. Error"],
    upper = ss_f[, "Estimate"] + 1.96 * ss_f[, "Std. Error"]
  )


  # Create AgeGroup and Year columns for ss_f
  ss_f$AgeGroup <- rep(paste0("Age group ", seq_len(nrow(F))), times = ncol(F))
  ss_f$Year <- rep(Year, each = nrow(F))



  confidence_intervals_f <- data.frame(
    AgeGroup = ss_f$AgeGroup,
    Year = ss_f$Year,
    estimate = ss_f[, "Estimate"],
    lower = ss_f[, "Estimate"] - 1.96 * ss_f[, "Std. Error"],
    upper = ss_f[, "Estimate"] + 1.96 * ss_f[, "Std. Error"]
  )




  p <- ggplot2::ggplot(confidence_intervals_f, aes(x = Year, y = estimate)) +
    ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
    ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
    ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
    ggplot2::labs(x = "Year", y = "Fishing Mortality", title = "Fishing Mortality Over Years with Confidence Intervals") +
    ggplot2::theme_minimal()



    }
  return(p)
}