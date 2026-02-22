#' Plot Catch Number (CN) or Catch Number by Age (CNA) over the years
#'
#' This function visualizes model outputs in the form of time-series plots of CN or CNA using ggplot2.
#' For CN, it plots a simple time-series. For CNA, it creates a separate time-series plot for each age group.
#'
#' @param model_result A list containing model output. The list should have a "report" component containing either a "CN" or "CNA" component.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. The color of the line in the plot. Default is "red".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @param se_color Character. The color of the confidence interval ribbon. Default is "red".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param se_type Character. Type of CI display: "ribbon" (shaded area) or "errorbar" (error bars). Default is "ribbon".
#' @param facet_ncol Numeric. Number of columns in facet wrap. Default is NULL.
#' @param facet_scales Character. Scales for facet wrap. Default is "free".
#' @param type Character vector. It specifies which type of catch is to be plotted: "CN" or "CNA". Default is c("CN","CNA").
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
#' @return A ggplot object representing the plot.
#'
#'
#' @examples
#' \dontrun{
#' # Use 'run_acl' to get 'model_result'
#' model_result <- run_acl(...)
#'
#' # Plot CN
#' plot_catch(model_result, type = "CN")
#'
#' # Plot CN with standard error
#' plot_catch(model_result, type = "CN", se = TRUE)
#'
#' # Plot CNA
#' plot_catch(model_result, type = "CNA")
#' }
#'
#' @export
plot_catch <- function(model_result, line_size = 1.5, line_color = "#D32F2F", line_type = "solid", se = FALSE, se_color = "#D32F2F", se_alpha = 0.2, se_type = c("ribbon", "errorbar"),facet_ncol = NULL, facet_scales = "free",type=c("CN","CNA"), return_data = FALSE, title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL){
  if(type=="CN")
    {
    # Extract the CN data
  CN <- model_result[["report"]][["CN"]]

  # Make sure it's a data frame
  if(!is.data.frame(CN)){
    CN <- as.data.frame(CN)
  }

  # Add Year to the CN data
  CN$Year <-  model_result[["year"]]

  if(!se)
  {
    # Plot CN over the years using ggplot2
    p <- ggplot2::ggplot(CN, aes(x = Year, y = CN)) +
      ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
      ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = "CN", title = if (!is.null(title)) title else .acl_title("CN")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

    data_out <-CN

  }


  else
  {
    # Filter rows that contain "B"
    ss_CN <- model_result[["est_std"]][grep("^CN", rownames(model_result[["est_std"]])),]

    # Calculate confidence intervals
    confidence_intervals_CN <- data.frame(
      estimate = ss_CN[, "Estimate"],
      lower = ss_CN[, "Estimate"] - 1.96 * ss_CN[, "Std. Error"],
      upper = ss_CN[, "Estimate"] + 1.96 * ss_CN[, "Std. Error"]
    )

    confidence_intervals_CN$Year <-  model_result[["year"]]

    # Plot recruitment over the years with confidence intervals using ggplot2
    p <- ggplot2::ggplot(confidence_intervals_CN, aes(x = Year, y = estimate)) +
      ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
      { if (se_type[1] == "ribbon")
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = se_color, alpha = se_alpha)
        else
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), color = se_color, width = 0.3) } +
      ggplot2::labs(y = "CN", x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), title = if (!is.null(title)) title else .acl_title("CN_se")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

    data_out <-confidence_intervals_CN

  }
  }
  if(type=="CNA"){

    # Extract the CNA data
    CNA<- model_result[["report"]][["CNA"]]

    # Create Year variable from column names of CNA (assuming columns are years)
    Year <- model_result[["year"]]

    # Create AgeGroup variable from row names of CNA
    AgeGroup <- factor(paste0("Age group ", seq_len(nrow(CNA))),
                       levels = paste0("Age group ", seq_len(nrow(CNA))))

    # Convert matrix to data frame in long format
    CNA_long <- reshape2::melt(CNA)
    colnames(CNA_long) <- c("AgeGroup", "Year", "CNA")
    CNA_long$Year <- Year[as.numeric(CNA_long$Year)]
    CNA_long$AgeGroup <- AgeGroup[as.numeric(CNA_long$AgeGroup)]


    # Plot catch numbers by age over the years using ggplot2
    p <- ggplot2::ggplot(CNA_long, aes(x = Year, y = CNA)) +
      ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
      ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
      ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = "Catch numbers by age", title = if (!is.null(title)) title else .acl_title("CNA")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

    data_out <-CNA_long

  }
  if (return_data) {
    return(list(plot = p, data = data_out))
  } else {
    return(p)
  }
}

