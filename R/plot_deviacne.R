#' Plot Deviation of Recruitment (R) and Fishing Mortality (F) Over Years
#'
#' This function calculates the deviations of R and F in the ACL model results,
#' which are used to measure the deviance of the model. These deviations are calculated
#' as the difference between the observed and expected values, with the model's residuals
#' as the measurement errors. This function can plot either R or F deviation over years,
#' depending on the 'type' parameter.
#'
#' @param model_result A list that contains the Annual Catch Limit (ACL) model output.
#' @param se Logical, whether to draw the standard error bar.
#' @param line_size Numeric, the line size.
#' @param line_color Character, the line color.
#' @param line_type Character, the line type.
#' @param point_color Character. The color of the point in the plot. Default is "white".
#' @param point_size Character. The size of the point in the plot. Default is "solid".
#' @param point_shape Character. The shape of the point in the plot. Default is "solid".
#' @param se_color Character, the color of the standard error.
#' @param se_width Numeric, the width of the standard error.
#' @param log Logical, whether to apply an exponential transformation to the results.
#' @param facet_ncol Number of columns in facet wrap. Default is NULL.
#' @param facet_scales Scales for facet wrap. Default is "free".
#' @param type Character. It specifies whether the deviation plot is for "R" or "F". Default is "R".
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
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{
#' # If you want to plot the deviation of recruitment (R) with standard error bars
#' plot_deviance(model_result = your_model_results, se = TRUE, type = "R",
#'               point_size = 1, point_color = "black", point_shape = 1,
#'               line_size = 1, line_color = "black", line_type = "solid",
#'               se_color = "blue", se_alpha = 0.4)
#'
#' # If you do not want to plot the standard error bars
#' plot_deviance(model_result = your_model_results, se = FALSE, type = "R",
#'               point_size = 1, point_color = "black", point_shape = 1,
#'               line_size = 1, line_color = "black", line_type = "solid")
#'
#' # If you want to plot the deviation of fishing mortality (F) with standard error bars
#' plot_deviance(model_result = your_model_results, se = TRUE, type = "F",
#'               point_size = 1, point_color = "black", point_shape = 1,
#'               line_size = 1, line_color = "black", line_type = "solid",
#'               se_color = "blue", se_alpha = 0.4)
#' }

plot_deviance <- function(model_result, se = TRUE, point_size=3,point_color="white",point_shape=21 ,line_size = 1, line_color = "black", line_type = "solid",se_color="black", se_width=0.5,facet_ncol = NULL, facet_scales = "free",log=T,type=c("R","F"), title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL) {

  if(type=="R"){
  dev_log_R<-model_result[["est_std"]][grep("^dev_log_R", rownames(model_result[["est_std"]])),]
  # Make sure it's a data frame
  if(!is.data.frame(dev_log_R)){
    dev_log_R <- as.data.frame(dev_log_R)
  }
  confidence_intervals_dev_log_R <- data.frame(
    estimate = dev_log_R[, "Estimate"],
    lower = dev_log_R[, "Estimate"] - 1.96 * dev_log_R[, "Std. Error"],
    upper = dev_log_R[, "Estimate"] + 1.96 * dev_log_R[, "Std. Error"]
  )


  # Add Year to the SSB data
  confidence_intervals_dev_log_R$Year <-  model_result[["year"]]


  # If log = FALSE, apply exp function to estimate, lower, and upper
  if (!log) {
    confidence_intervals_dev_log_R[, c("estimate", "lower", "upper")] <- exp(confidence_intervals_dev_log_R[, c("estimate", "lower", "upper")])
  }



   plot <- ggplot2::ggplot(confidence_intervals_dev_log_R, ggplot2::aes(x = Year, y = estimate)) +
    ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
    ggplot2::geom_point(size=point_size,fill =point_color,shape=point_shape) +
    ggplot2::labs(y = "Recruitment deviance", x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), title = if (!is.null(title)) title else .acl_title("dev_R")) +
    .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)



  if (se) {


    plot <- plot + ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), color = se_color, width = se_width)+
      ggplot2::geom_point(size=point_size,fill=point_color,shape=point_shape)

  }
  }
  if(type=="F"){

    dev_log_F<-model_result[["est_std"]][grep("^dev_log_F", rownames(model_result[["est_std"]])),]
    dev_log_F<-as.data.frame(dev_log_F)
    # Calculate confidence intervals
    confidence_intervals_dev_log_F <- data.frame(
      estimate = dev_log_F[, "Estimate"],
      lower = dev_log_F[, "Estimate"] - 1.96 * dev_log_F[, "Std. Error"],
      upper = dev_log_F[, "Estimate"] + 1.96 * dev_log_F[, "Std. Error"]
    )


    # Create AgeGroup and Year columns for ss_f
    F<- model_result[["report"]][["F"]]

    age_levels <- paste0("Age group ", seq_len(nrow(F)))
    dev_log_F$AgeGroup <- factor(rep(age_levels, times = ncol(F)), levels = age_levels)
    dev_log_F$Year <- rep(model_result[["year"]], each = nrow(F))



    confidence_intervals_dev_log_F <- data.frame(
      AgeGroup = dev_log_F$AgeGroup,
      Year = dev_log_F$Year,
      estimate = dev_log_F[, "Estimate"],
      lower = dev_log_F[, "Estimate"] - 1.96 * dev_log_F[, "Std. Error"],
      upper = dev_log_F[, "Estimate"] + 1.96 * dev_log_F[, "Std. Error"]
    )


    # If log = FALSE, apply exp function to estimate, lower, and upper
    if (!log) {
      confidence_intervals_dev_log_F[, c("estimate", "lower", "upper")] <- exp(confidence_intervals_dev_log_F[, c("estimate", "lower", "upper")])
    }



    plot <- ggplot2::ggplot(confidence_intervals_dev_log_F, ggplot2::aes(x = Year, y = estimate)) +
      ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_point(size=point_size,fill =point_color,shape=point_shape) +
      ggplot2::labs(y = "F deviance", x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), title = if (!is.null(title)) title else .acl_title("dev_F")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)+
      ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales)




    if (se) {


      plot <- plot + ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), color = se_color, width = se_width)+
        ggplot2::geom_point(size=point_size,fill=point_color,shape=point_shape)

    }

  }
  return(plot)
}
















