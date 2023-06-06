#' Plot NL over the years
#'
#' This function takes a list and a data frame as input, and plots NL (counts in length groups) over the years using ggplot2. The plot is faceted by the length groups.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "NL" component representing counts in length groups.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.5.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param facet_ncol Number of columns in facet wrap. Default is 3.
#' @param facet_scales Scales for facet wrap. Default is "free".
#' @param se_color Character. The color of the confidence interval ribbon. Default is "blue".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @export
#' @examples
#' \dontrun{
#' plot_NL(model_result, data.CatL)
#' }
plot_NL <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid",facet_ncol = 3, facet_scales = "free" ,se = FALSE, se_color = "red", se_alpha = 0.2){

  # Extract the NL data
  NL <- model_result[["report"]][["NL"]]

  # Make sure it's a matrix
  if(!is.matrix(NL)){
    NL <- as.matrix(NL)
  }

  # Create Year variable from column names of NL (assuming columns are years)
  Year <- model_result[["year"]]

  # Create LengthGroup variable from row names of NL
  LengthGroup <- paste0("Length bin ", seq_len(nrow(NL)))

  # Convert matrix to data frame in long format
  NL_long <- reshape2::melt(NL)
  colnames(NL_long) <- c("LengthGroup", "Year", "Count")
  NL_long$Year <- Year[match(NL_long$Year, 1:length(Year))]
  NL_long$LengthGroup <- LengthGroup[as.numeric(NL_long$LengthGroup)]

  if(!se)
    {
  # Plot NL over the years using ggplot2
  p <- ggplot2::ggplot(NL_long, aes(x = Year, y = Count)) +
    ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
    ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
    ggplot2::labs(x = "Year", y = "Relative abundance", title = "NL Over Years") +
    ggplot2::theme_minimal()
  }
  else{

    # Filter rows that contain "NL"
    ss_NL <- model_result[["est_std"]][grep("^NL", rownames(model_result[["est_std"]])),]

    ss_NL<-as.data.frame(ss_NL)
    # Calculate confidence intervals
    confidence_intervals_NL <- data.frame(
      estimate = ss_NL[, "Estimate"],
      lower = ss_NL[, "Estimate"] - 1.96 * ss_NL[, "Std. Error"],
      upper = ss_NL[, "Estimate"] + 1.96 * ss_NL[, "Std. Error"]
    )


    # Create LengthGroup and Year columns for ss_NL
    ss_NL$LengthGroup <- rep(paste0("Lenth bin ", seq_len(nrow(NL))), times = ncol(NL))
    ss_NL$Year <- rep(Year, each = nrow(NL))



    confidence_intervals_NL <- data.frame(
      LengthGroup = ss_NL$LengthGroup,
      Year = ss_NL$Year,
      estimate = ss_NL[, "Estimate"],
      lower = ss_NL[, "Estimate"] - 1.96 * ss_NL[, "Std. Error"],
      upper = ss_NL[, "Estimate"] + 1.96 * ss_NL[, "Std. Error"]
    )




    p <- ggplot2::ggplot(confidence_intervals_NL, aes(x = Year, y = estimate)) +
      ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
      ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
      ggplot2::labs(x = "Year", y = "Relative abundance", title = "NL Over Years with Confidence Intervals") +
      ggplot2::theme_minimal()



  }
  return(p)
}
