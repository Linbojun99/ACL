#' @title Plot the Distribution of Model Residuals from an Age-Structured Assessment Model (ACL)
#'
#' @description This function takes the output from the `run_acl` function and creates a facet plot of the residuals for each index.
#' The residuals reflect the difference between the observed and expected values from the ACL model, a type of age-structured
#' assessment model commonly used in fishery science. The function can handle two types of residuals:
#' 1) "length", where residuals are calculated over length groups for each year, and
#' 2) "year", where residuals are calculated over years for each length group.
#' Each index is represented in a separate facet.
#'
#' @param model_result A list obtained from the `run_acl` function. This list should contain a "report" component that includes
#' a "resid_index" component, representing the residuals for each index.
#' @param f Numeric. The smoother span for the loess smooth line in the plot. This gives the proportion of points in the plot which
#' influence the smooth at each value. Larger values result in more smoothing. Default is 0.4.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param smooth_color Character. The color of the smooth line in the plot. Default is "blue".
#' @param hline_color Character. The color of the horizontal line (at y=0) in the plot. Default is "red".
#' @param line_size Numeric. The size of the lines in the plot. Default is 1.
#' @param facet_ncol Numeric, optional. Number of columns in facet wrap.  Default is NULL.
#' @param facet_scales Character. The "scales" argument for the facet_wrap function in ggplot2. Default is "free".
#' @param type Character. Specifies whether the residuals are calculated over "length" or "year". Default is "length".
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
#' @return A ggplot object representing the facet plot of residuals.
#'
#' @examples
#' \dontrun{
#' # Use 'run_acl' to get 'model_result'
#' model_result <- run_acl(...)
#'
#' # Call the function with type = "length"
#' p_length <- plot_residuals(model_result, type = "length")
#'
#' # Print the plot for "length"
#' print(p_length)
#'
#' # Call the function with type = "year"
#' p_year <- plot_residuals(model_result, type = "year")
#'
#' # Print the plot for "year"
#' print(p_year)
#' }
#' @export
plot_residuals <- function(model_result, f = 0.4, line_color = "black", smooth_color = "blue", hline_color = "red", line_size = 1, facet_scales = "free", facet_ncol=NULL,type=c("length","year"), return_data = FALSE, title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL) {

   len_label=model_result[["len_label"]]

   if(type=="length"){

    fixed_len_labels <- .acl_fix_len_labels(len_label)
    num_indices <- dim(model_result[["report"]][["resid_index"]])[1]
    plot_data <- data.frame()
    for(i in seq_len(num_indices)){
      temp_data <- as.data.frame(model_result[["report"]][["resid_index"]][i,])

      temp_data$LengthGroup <- fixed_len_labels[i]

      temp_data$year <- model_result[["year"]]
      plot_data <- rbind(plot_data, temp_data)
    }
    colnames(plot_data) <- c("residual", "LengthGroup","year")
    plot_data$LengthGroup <- factor(plot_data$LengthGroup,
                                    levels = fixed_len_labels)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x=year, y=residual)) +
      ggplot2::geom_line(color = line_color, linewidth = line_size) +
      ggplot2::geom_smooth(method="loess", formula=y~x, se=FALSE, color=smooth_color, linetype=2, linewidth=line_size) +
      ggplot2::geom_hline(yintercept=0, color=hline_color, linewidth=line_size) +
      ggplot2::facet_wrap(~LengthGroup, scales = facet_scales)+
      .acl_scale_x(x_breaks, n_breaks = 8) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)+
      ggplot2::labs(x=.acl_lab("x","year"),y=.acl_lab("y","residual"),title=.acl_title("resid_length"))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    data_out <-plot_data

  }
  if (type=="year")
  {
    # Define the data
    resid_index <- model_result[["report"]][["resid_index"]]
    year <- model_result[["year"]]
    len_mid <- model_result[["len_mid"]]
    # Transform the matrix into a data frame and add column names
    df <- as.data.frame(resid_index)
    colnames(df) <- year
    df$len_mid <- len_mid
    # Reshape the data from wide to long format
    df_long <- df %>%
      tidyr::pivot_longer(-len_mid, names_to = "year", values_to = "residuals")
    colnames(df_long) <- c("length", "Year","residual")
    df_long$Year <- factor(paste("Year", df_long$Year),
                           levels = paste("Year", sort(unique(as.numeric(df_long$Year)))))
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x=length, y=residual)) +
      ggplot2::geom_line(color = line_color, linewidth = line_size) +
      ggplot2::geom_smooth(method="loess", formula=y~x, se=FALSE, color=smooth_color, linetype=2, linewidth=line_size) +
      ggplot2::geom_hline(yintercept=0, color=hline_color, linewidth=line_size) +
      ggplot2::facet_wrap(~Year, scales = facet_scales,ncol = facet_ncol)+
      .acl_scale_x(x_breaks, n_breaks = 8) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)+ggplot2::labs(x=.acl_lab("x","length"),y=.acl_lab("y","residual"),title=.acl_title("resid_year"))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    data_out <-df_long

  }
   if (return_data) {
     return(list(plot = p, data = data_out))
   } else {
     return(p)
   }
}
