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
#' @param se_type Character. Type of CI display: "ribbon" (shaded area) or "errorbar" (error bars). Default is "ribbon".
#' @param facet_ncol Integer. The number of columns in facet_wrap.
#' @param facet_scales Character. Scales for facet_wrap. Default is "free".
#' @param type Character. Specifies whether to plot "year"  or "age" . Default is "year".
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
plot_fishing_mortality <- function(model_result, line_size = 1, line_color = "#D32F2F", line_type = "solid", facet_ncol = NULL,
                                   facet_scales = "free" ,se = FALSE, se_color = "#D32F2F", se_alpha = 0.2, se_type = c("ribbon", "errorbar"),type=c("year","age"), return_data = FALSE, title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL){


  if(type=="year"){
    # Extract the F data
    F<- model_result[["report"]][["F"]]
    # Create Year variable from column names of F (assuming columns are years)
    Year <- model_result[["year"]]
    # Create AgeGroup variable from row names of F
    AgeGroup <- factor(paste0("Age group ", seq_len(nrow(F))),
                       levels = paste0("Age group ", seq_len(nrow(F))))

    # Convert matrix to data frame in long format
    F_long <- reshape2::melt(F)
    colnames(F_long) <- c("AgeGroup", "Year", "F")
    F_long$Year <- Year[as.numeric(F_long$Year)]
    F_long$AgeGroup <- AgeGroup[as.numeric(F_long$AgeGroup)]

    if(!se)
    {
      # Plot fishing mortality over the years using ggplot2
      p <- ggplot2::ggplot(F_long, aes(x = Year, y = F)) +
        ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
        ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "F"), title = if (!is.null(title)) title else .acl_title("F_year")) +
        .acl_scale_x(x_breaks, n_breaks = 10) +
        .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

      data_out <-F_long

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
      age_levels <- paste0("Age group ", seq_len(nrow(F)))
      ss_f$AgeGroup <- factor(rep(age_levels, times = ncol(F)), levels = age_levels)
      ss_f$Year <- rep(Year, each = nrow(F))



      confidence_intervals_f <- data.frame(
        AgeGroup = ss_f$AgeGroup,
        Year = ss_f$Year,
        estimate = ss_f[, "Estimate"],
        lower = ss_f[, "Estimate"] - 1.96 * ss_f[, "Std. Error"],
        upper = ss_f[, "Estimate"] + 1.96 * ss_f[, "Std. Error"]
      )




      p <- ggplot2::ggplot(confidence_intervals_f, aes(x = Year, y = estimate)) +
        ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
        { if (se_type[1] == "ribbon")
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = se_color, alpha = se_alpha)
        else
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), color = se_color, width = 0.3) } +
        ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "F"), title = if (!is.null(title)) title else .acl_title("F_year_se")) +
        .acl_scale_x(x_breaks, n_breaks = 10) +
        .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

      data_out <-confidence_intervals_f


    }
  }

  if(type=="age"){
    # Extract the F data
    F<- model_result[["report"]][["F"]]
    # Create Year variable from column names of F (assuming columns are years)
    Year <- model_result[["year"]]
    # Create AgeGroup variable from row names of F
    AgeGroup <- paste0("Age group ", seq_len(nrow(F)))
    #AgeGroup <- factor(paste("Age bin ", seq_len(nrow(F))), levels=paste("Age bin ",seq_len(nrow(F))))

    # Convert matrix to data frame in long format
    F_long <- reshape2::melt(F)
    colnames(F_long) <- c("AgeGroup", "Year", "F")
    F_long$Year <- Year[as.numeric(F_long$Year)]
    #F_long$AgeGroup <- AgeGroup[as.numeric(F_long$AgeGroup)]

    if(!se)
    {
      # Plot fishing mortality over the years using ggplot2
      p <- ggplot2::ggplot(F_long, aes(x = AgeGroup, y = F)) +
        ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
        ggplot2::facet_wrap(~Year, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = "AgeGroup", y = if (!is.null(ylab)) ylab else .acl_lab("y", "F"), title = if (!is.null(title)) title else .acl_title("F_age")) +
        .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

      data_out <-F_long

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
      ss_f$AgeGroup <- rep(paste0("", seq_len(nrow(F))), times = ncol(F))
      ss_f$Year <- rep(Year, each = nrow(F))



      confidence_intervals_f <- data.frame(
        AgeGroup = ss_f$AgeGroup,
        Year = ss_f$Year,
        estimate = ss_f[, "Estimate"],
        lower = ss_f[, "Estimate"] - 1.96 * ss_f[, "Std. Error"],
        upper = ss_f[, "Estimate"] + 1.96 * ss_f[, "Std. Error"]
      )


      confidence_intervals_f$AgeGroup=as.numeric(confidence_intervals_f$AgeGroup )

      p <- ggplot2::ggplot(confidence_intervals_f, aes(x = AgeGroup, y = estimate)) +
        ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
        { if (se_type[1] == "ribbon")
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = se_color, alpha = se_alpha)
        else
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), color = se_color, width = 0.3) } +
        ggplot2::facet_wrap(~Year, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = "AgeGroup", y = if (!is.null(ylab)) ylab else .acl_lab("y", "F"), title = if (!is.null(title)) title else .acl_title("F_age_se")) +
        .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)


      data_out <-confidence_intervals_f

    }
  }

  if (return_data) {
    return(list(plot = p, data = data_out))
  } else {
    return(p)
  }
}
