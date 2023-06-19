#' Plot biomass over the years
#'
#' This function takes a list and a data frame as input, and plots biomass over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "B" component representing biomass.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se_color Character. The color of the confidence interval ribbon. Default is "blue".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @param facet_ncol Integer, number of columns in facet_wrap.
#' @param facet_scales Character, scales for facet_wrap.
#' @param type Character. It specifies which the biomass is plot for "B" , "BL" . Default is "N".
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' plot_biomass(model_result, line_size = 1.2, line_color = "red", line_type = "solid")
#' }
plot_biomass <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2,type=c("B","BL"),facet_ncol = 3, facet_scales = "free"){
  if(type=="B"){

  # Extract the biomass data
  biomass <- model_result[["report"]][["B"]]

  # Make sure it's a data frame
  if(!is.data.frame(biomass)){
    biomass <- as.data.frame(biomass)
  }

  # Add Year to the biomass data
  biomass$Year <-  model_result[["year"]]

if(!se)
  {
  # Plot biomass over the years using ggplot2
  p <- ggplot2::ggplot(biomass, aes(x = Year, y = biomass)) +
    ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
    ggplot2::labs(x = "Year", y = "Biomass", title = "Biomass Over Years") +
    ggplot2::theme_minimal()
}


  else
  {
    # Filter rows that contain "B"
    ss_bio <- model_result[["est_std"]][grep("^B", rownames(model_result[["est_std"]])),]

    # Calculate confidence intervals
    confidence_intervals_bio <- data.frame(
      estimate = ss_bio[, "Estimate"],
      lower = ss_bio[, "Estimate"] - 1.96 * ss_bio[, "Std. Error"],
      upper = ss_bio[, "Estimate"] + 1.96 * ss_bio[, "Std. Error"]
    )

    confidence_intervals_bio$Year <-  model_result[["year"]]

    # Plot recruitment over the years with confidence intervals using ggplot2
    p <- ggplot2::ggplot(confidence_intervals_bio, aes(x = Year, y = estimate)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
      ggplot2::labs(y = "Biomass", x = "Year", title = "Biomass Over Years with Confidence Intervals") +
      ggplot2::theme_minimal()
  }
  }
  if (type=="BL"){

    # Extract the BL data
    BL <- model_result[["report"]][["BL"]]

    # Make sure it's a matrix
    if(!is.matrix(BL)){
      BL <- as.matrix(BL)
    }

    # Create Year variable from column names of BL (assuming columns are years)
    Year <- model_result[["year"]]

    # Create LengthGroup variable from row names of BL
    LengthGroup <- paste0("Length bin ", seq_len(nrow(BL)))
    LengthGroup <- factor(paste("Length bin ", seq_len(nrow(BL))), levels=paste("Length bin ", seq_len(nrow(BL))))

    # Convert matrix to data frame in long format
    BL_long <- reshape2::melt(BL)
    colnames(BL_long) <- c("LengthGroup", "Year", "Count")
    BL_long$Year <- Year[match(BL_long$Year, 1:length(Year))]
    BL_long$LengthGroup <- LengthGroup[as.numeric(BL_long$LengthGroup)]




    # Plot BL over the years using ggplot2
    p <- ggplot2::ggplot(BL_long, aes(x = Year, y = Count)) +
      ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
      ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
      ggplot2::labs(x = "Year", y = "Relative abundance", title = "BL Over Years") +
      ggplot2::theme_minimal()

  }
  return(p)
}

