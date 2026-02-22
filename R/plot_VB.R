#' Plot the Von Bertalanffy growth function
#'
#' This function takes Linf, k, and t0 as input, creates a sequence of ages, applies the VB function to each age, and then plots the result using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains "Linf", "vbk" and "t0" components.
#' @param age_range Numeric vector of length 2, defining the range of ages to consider.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se_color Character. The color of the confidence interval ribbon. Default is "blue".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param se_type Character. Type of CI display: "ribbon" (shaded area) or "errorbar" (error bars). Default is "ribbon".
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @param text_size Numeric. The thickness of the text in the plot. Default is 5.
#' @param text_color Character. The color of the text in the plot. Default is "black".
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
#' @export
#' @examples
#' \dontrun{
#' plot_VB(model_result, age_range = c(1, 25))
#' }
plot_VB <- function(model_result, age_range = c(1, 25), line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2, se_type = c("ribbon", "errorbar"),text_color="black",text_size=5, title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL){
  # Define the VB function
  VB_func <- function(Linf, k, t0, age) {
    Lt = Linf * (1 - exp(-k * (age - t0)))
    return(Lt)
  }

  Linf = model_result[["report"]][["Linf"]]
  k = model_result[["report"]][["vbk"]]
  t0 = model_result[["report"]][["t0"]]

  # Create a data frame with a sequence of ages and the corresponding lengths
  data <- data.frame(age = seq(age_range[1], age_range[2], by = 1))
  data <- data %>%
    mutate(length = VB_func(Linf, k, t0, age))





  if (!se)
    {
  # Plot the VB function using ggplot2
  p <- ggplot2::ggplot(data, aes(x = age, y = length)) +
    ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
    ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "age"), y = "Length", title = if (!is.null(title)) title else .acl_title("VB")) +
    ggplot2::annotate("text", x = age_range[1] + 1, y = max(data$length) - 1,
                      label = paste("Linf = ", round(Linf, 2), "\nk = ", round(k, 2)),
              hjust = 0, vjust = 1, size = 5, color = "black") +
    .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

}
  else{

  ss_logk<-model_result[["est_std"]][grep("^log_vbk", rownames(model_result[["est_std"]])),]
  ss_logk<-as.data.frame(ss_logk)
  ss_k <- data.frame(
    estimate = exp(ss_logk[ "Estimate",]),
    lower = exp(ss_logk[ "Estimate",] - 1.96 * ss_logk[ "Std. Error",]),
    upper = exp(ss_logk[ "Estimate",] + 1.96 * ss_logk["Std. Error", ])
  )


  data <- data.frame(age = seq(age_range[1], age_range[2], by = 1))
  data <- data %>%
    mutate(
      length = VB_func(Linf, k, t0, age),
      length_lower = VB_func(Linf, ss_k$lower, t0, age),
      length_upper = VB_func(Linf, ss_k$upper, t0, age)
    )

  # Plot the VB function using ggplot2
  p <- ggplot2::ggplot(data, aes(x = age, y = length)) +
    { if (se_type[1] == "ribbon")
          ggplot2::geom_ribbon(ggplot2::aes(ymin = length_lower, ymax = length_upper), alpha = se_alpha, fill = se_color)
        else
          ggplot2::geom_errorbar(ggplot2::aes(ymin = length_lower, ymax = length_upper), color = se_color, width = 0.3) } +
    ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
    ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "age"), y = "Length", title = if (!is.null(title)) title else .acl_title("VB")) +
    ggplot2::annotate("text", x = age_range[1] + 1, y = max(data$length_upper) - 1,
                      label = paste("Linf = ", round(Linf, 2), "\nk = ", round(k, 2)),
                       hjust = 0, vjust = 1, size = text_size, color = text_color) +
    .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)


}

  return(p)
}
