#' @title Plot Biomass Over Years from an Age-Structured Assessment Model (ACL)
#'
#' @description This function takes the output from the `run_acl` function and plots biomass over the years using ggplot2.
#' The biomass can be of two types, "B", representing the absolute biomass, and "BL", representing the biomass distributed
#' over length groups for each year.
#'
#' @param model_result A list obtained from the `run_acl` function. This list should contain a "report" component that includes
#' a "B" component (absolute biomass) and a "BL" component (length-grouped biomass).
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. The color of the line in the plot. Default is "red".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE. If TRUE, standard error
#' will be calculated and confidence intervals will be shown as a shaded area around the line.
#' @param se_color Character. The color of the confidence interval ribbon. Default is "red".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param type Character. Specifies whether to plot "B" (absolute biomass) or "BL" (length-grouped biomass). Default is "B".
#' @param facet_ncol Integer. The number of columns in facet_wrap. Only applicable when type = "BL".
#' @param facet_scales Character. Scales for facet_wrap. Only applicable when type = "BL".
#'
#' @return A ggplot object representing the plot of biomass over years.
#'
#' @examples
#' \dontrun{
#' # Use 'run_acl' to get 'model_result'
#' model_result <- run_acl(...)
#'
#' # Call the function with type = "B", standard error set to TRUE
#' p_biomass_B <- plot_biomass(model_result, type = "B", se = TRUE)
#'
#' # Print the plot for "B" with standard error
#' print(p_biomass_B)
#'
#' # Call the function with type = "BL"
#' p_biomass_BL <- plot_biomass(model_result, type = "BL")
#'
#' # Print the plot for "BL"
#' print(p_biomass_BL)
#' }
#' @export
plot_biomass <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2,type=c("B","BL"),facet_ncol = NULL, facet_scales = "free"){
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
      ggplot2::labs(x = "Year", y = "Relative abundance", title = "Biomass at length group Over Years") +
      ggplot2::theme_minimal()

  }
  return(p)
}

