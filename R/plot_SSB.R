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
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param se_type Character. Type of CI display: "ribbon" (shaded area) or "errorbar" (error bars). Default is "ribbon".
#' @param type Character, optional. It specifies whether to plot for "SSB" or "SBL". Default is "SSB".
#' @param facet_ncol Numeric, optional. Number of columns in facet wrap. This parameter is only applicable when type is "SBL". Default is NULL.
#' @param facet_scales Character, optional. Scales for facet wrap. This parameter is only applicable when type is "SBL". Default is "free".
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
plot_SSB <- function(model_result, line_size = 1.5, line_color = "#D32F2F", line_type = "solid", se = FALSE, se_color = "#D32F2F", se_alpha = 0.2, se_type = c("ribbon", "errorbar"),type=c("SSB","SBL"),facet_ncol = NULL, facet_scales = "free", return_data = FALSE, title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL){

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
      ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
      ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = "Relative biomass", title = if (!is.null(title)) title else .acl_title("SSB")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

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
      ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
      { if (se_type[1] == "ribbon")
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = se_color, alpha = se_alpha)
        else
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), color = se_color, width = 0.3) } +
      ggplot2::labs(y = "Relative biomass", x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), title = if (!is.null(title)) title else .acl_title("SSB_se")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

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
    SBL_long$LengthGroup <- factor(.acl_fix_len_labels(len_label),
                                    levels = .acl_fix_len_labels(len_label))

    # Plot SBL over the years using ggplot2
    p <- ggplot2::ggplot(SBL_long, aes(x = Year, y = Count)) +
      ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
      ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
      ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = "Relative biomass", title = if (!is.null(title)) title else .acl_title("SBL")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

    data_out <-SBL_long

  }
  if (return_data) {
    return(list(plot = p, data = data_out))
  } else {
    return(p)
  }
}

