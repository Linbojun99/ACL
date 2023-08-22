#' @title Plot Spawning Stock Biomass (SSB) or Spawning Biomass at Length (SBL) Over the Years
#'
#' @description This function takes a list and a data frame as input, and plots SSB or SBL over the years using ggplot2.
#'
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "SSB" or "SBL" component representing Spawning Stock Biomass or Spawning Biomass at Length respectively.
#' @param line_size Numeric, optional. The thickness of the line in the plot. Default is 1.2.
#' @param line_color Character, optional. The color of the line in the plot. Default is "red".
#' @param line_type Character, optional. The type of the line in the plot. Default is "solid".
#' @param se Logical, optional. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @param se_color Character, optional. The color of the confidence interval ribbon. Default is "red".
#' @param se_alpha Numeric, optional. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param type Character, optional. It specifies whether to plot for "SSB" or "SBL". Default is "SSB".
#' @param facet_ncol Numeric, optional. Number of columns in facet wrap. This parameter is only applicable when type is "SBL". Default is NULL.
#' @param facet_scales Character, optional. Scales for facet wrap. This parameter is only applicable when type is "SBL". Default is "free".
#' @param return_data A logical indicating whether to return the processed data alongside the plot. Default is FALSE.
#'
#' @return A ggplot object representing the plot of Spawning Stock Biomass or Spawning Biomass at Length over the years.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use 'run_acl' to get 'model_result'
#' model_result <- run_acl(...)
#'
#'
#' # Plot SSB with confidence intervals
#' plot_SSB(model_result = model_result, type = "SSB", se = TRUE)
#'
#' # Plot SSB without confidence intervals
#' plot_SSB(model_result = model_result, type = "SSB", se = FALSE)
#'
#' # Plot SBL with confidence intervals
#' plot_SSB(model_result = model_result, type = "SBL", se = TRUE)
#'
#' # Plot SBL without confidence intervals
#' plot_SSB(model_result = model_result, type = "SBL", se = FALSE)
#' }
plot_SSB <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2,type=c("SSB","SBL"),facet_ncol = NULL, facet_scales = "free", return_data = FALSE){

  len_label=model_result[["len_label"]]

  if(type=="SSB"){


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
      ggplot2::labs(x = "Year", y = "Relative biomass", title = "SSB Over Years") +
      ggplot2::theme_minimal()

    data_out <-SSB
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
      ggplot2::labs(y = "Relative biomass", x = "Year", title = "SSB Over Years with Confidence Intervals") +
      ggplot2::theme_minimal()

    data_out <-confidence_intervals_ssb
  }
 }
  if(type=="SBL"){
    # Extract the SBL data
    SBL <- model_result[["report"]][["SBL"]]

    # Make sure it's a matrix
    if(!is.matrix(SBL)){
      SBL <- as.matrix(SBL)
    }

    # Create Year variable from column names of SBL (assuming columns are years)
    colnames(SBL) <- as.character(model_result[["year"]])
    Year <- model_result[["year"]]



    # Convert matrix to data frame in long format
    SBL_long <- reshape2::melt(SBL)
    colnames(SBL_long) <- c("LengthGroup", "Year", "Count")
    SBL_long$LengthGroup <- paste("Length bin", LengthGroup)

    # Plot SBL over the years using ggplot2
    p <- ggplot2::ggplot(SBL_long, aes(x = Year, y = Count)) +
      ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
      ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
      ggplot2::labs(x = "Year", y = "Relative biomass", title = "SBL Over Years") +
      ggplot2::theme_minimal()

    data_out <-SBL_long

  }
  if (return_data) {
    return(list(plot = p, data = data_out))
  } else {
    return(p)
  }
}

