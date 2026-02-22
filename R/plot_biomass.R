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
#' @param se_type Character. Type of CI display: "ribbon" (shaded area) or "errorbar" (error bars). Default is "ribbon".
#' @param type Character. Specifies whether to plot "B" (absolute biomass) or "BL" (length-grouped biomass). Default is "B".
#' @param facet_ncol Integer. The number of columns in facet_wrap. Only applicable when type = "BL".
#' @param facet_scales Character. Scales for facet_wrap. Only applicable when type = "BL".
#' @param return_data A logical indicating whether to return the processed data alongside the plot. Default is FALSE.
#'
#' @param title Character or NULL. Custom plot title. If NULL, uses global theme setting. See \code{acl_theme_set()}.
#' @param xlab Character or NULL. Custom x-axis label. If NULL, uses global theme setting.
#' @param ylab Character or NULL. Custom y-axis label. If NULL, uses global theme setting.
#' @param font_family Character or NULL. Custom font family. If NULL, uses global theme setting (default "Arial").
#' @param title_size Numeric or NULL. Plot title size in pt. If NULL, uses global theme (default 14).
#' @param axis_title_size Numeric or NULL. Axis title size in pt. If NULL, uses global theme (default 12).
#' @param axis_text_size Numeric or NULL. Axis tick label size in pt. If NULL, uses global theme (default 10).
#' @param strip_text_size Numeric or NULL. Facet label size in pt. If NULL, uses global theme (default 10).
#' @param legend_text_size Numeric or NULL. Legend text size in pt. If NULL, uses global theme (default 10).
#' @param x_breaks Numeric vector or NULL. Custom x-axis breaks (e.g. \code{seq(1, 20, by = 2)}). NULL = auto.
#' @param base_theme Character or NULL. Base ggplot2 theme name (e.g. "theme_bw"). NULL = global setting.
#' @param title_hjust Numeric or NULL. Title horizontal alignment: 0 = left, 0.5 = center, 1 = right. NULL = global setting.
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
plot_biomass <- function(model_result, line_size = 1.5, line_color = "#D32F2F", line_type = "solid", se = FALSE, se_color = "#D32F2F", se_alpha = 0.2, se_type = c("ribbon", "errorbar"),type=c("B","BL"),facet_ncol = NULL, facet_scales = "free", return_data = FALSE, title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL){

  len_label=model_result[["len_label"]]

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
    ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
    ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = "Relative biomass", title = if (!is.null(title)) title else .acl_title("B")) +
    .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

  data_out <-biomass

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
      ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
      { if (se_type[1] == "ribbon")
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = se_color, alpha = se_alpha)
        else
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), color = se_color, width = 0.3) } +
      ggplot2::labs(y = "Relative biomass", x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), title = if (!is.null(title)) title else .acl_title("B_se")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

    data_out <-confidence_intervals_bio

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


    # Convert matrix to data frame in long format
    BL_long <- reshape2::melt(BL)
    colnames(BL_long) <- c("LengthGroup", "Year", "Count")
    BL_long$Year <- Year[match(BL_long$Year, 1:length(Year))]
    BL_long$LengthGroup <- factor(.acl_fix_len_labels(len_label),
                                   levels = .acl_fix_len_labels(len_label))




    # Plot BL over the years using ggplot2
    p <- ggplot2::ggplot(BL_long, aes(x = Year, y = Count)) +
      ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
      ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
      ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = "Relative biomass", title = if (!is.null(title)) title else .acl_title("BL")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

    data_out <-BL_long

  }
  if (return_data) {
    return(list(plot = p, data = data_out))
  } else {
    return(p)
  }
}

