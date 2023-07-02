#' @title Plot Fishing Mortality Over Years from an Age-Structured Assessment Model (ACL)
#'
#' @description This function takes the output from the `run_acl` function and plots fishing mortality over the years using ggplot2.
#' The function can also calculate and display the standard error as confidence intervals.
#'
#' @param model_result A list obtained from the `run_acl` function. This list should contain a "report" component that includes
#' a "F" component representing fishing mortality for each age group over years.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.
#' @param line_color Character. The color of the line in the plot. Default is "red".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE. If TRUE, standard error
#' will be calculated and confidence intervals will be shown as a shaded area around the line.
#' @param se_color Character. The color of the confidence interval ribbon. Default is "red".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param facet_ncol Integer. The number of columns in facet_wrap.
#' @param facet_scales Character. Scales for facet_wrap. Default is "free".
#'
#' @return A ggplot object representing the plot of fishing mortality over years.
#'
#' @examples
#' \dontrun{
#' # Use 'run_acl' to get 'model_result'
#' model_result <- run_acl(...)
#'
#' # Call the function with standard error set to TRUE
#' p_mortality <- plot_fishing_mortality(model_result, se = TRUE)
#'
#' # Print the plot with standard error
#' print(p_mortality)
#'
#' # Call the function with standard error set to FALSE
#' p_mortality <- plot_fishing_mortality(model_result, se = FALSE)
#'
#' # Print the plot without standard error
#' print(p_mortality)
#' }
#' @export
plot_fishing_mortality <- function(model_result, line_size = 1, line_color = "red", line_type = "solid", facet_ncol = NULL, facet_scales = "free" ,se = FALSE, se_color = "red", se_alpha = 0.2){

  # Extract the F data
  F<- model_result[["report"]][["F"]]

  # Create Year variable from column names of F (assuming columns are years)
  Year <- model_result[["year"]]

  # Create AgeGroup variable from row names of F
  AgeGroup <- paste0("Age group ", seq_len(nrow(F)))
  AgeGroup <- factor(paste("Age bin ", seq_len(nrow(F))), levels=paste("Age bin ",seq_len(nrow(F))))

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
