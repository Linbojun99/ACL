#' @title Plot the Relationship between Spawning Stock Biomass (SSB) and Recruitment (Rec)
#'
#' @description This function takes the model result as an input and creates a scatter plot to visualize the relationship between
#' Spawning Stock Biomass (SSB) and Recruitment (Rec). These metrics are fundamental in the context of fishery stock assessment.
#'
#' @param model_result A list containing the model results. It includes a component "report" that comprises "SSB" and "Rec".
#' @param age_at_recruitment Numeric. The age at which individuals are recruited. Used to align SSB with Rec. Default is 1.
#' @param point_color Character. The color of the points in the scatter plot. Default is "black".
#' @param point_size Numeric. The size of the points in the scatter plot. Default is 2.
#' @param point_shape Numeric. The shape of the points in the scatter plot, as an integer value (see ?points in base R for more info). Default is 16 (filled circle).
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
#' @return A ggplot object representing the scatter plot of SSB versus Recruitment.
#'
#' @examples
#' \dontrun{
#' # The model_result is typically obtained from run_acl().
#' # Here, we create a dummy example.
#' # Use 'run_acl' to get 'model_result'
#' model_result <- run_acl(...)
#'
#' # Call the function
#' p <- plot_SSB_Rec(model_result)
#'
#' # Print the plot
#' print(p)
#' }
#' @export
plot_SSB_Rec <- function(model_result, age_at_recruitment = 1,
                         point_size=2,point_color="black",point_shape=16, return_data = FALSE, title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL) {


  # Extract the necessary data
  SSB_data <- model_result[["report"]][["SSB"]]
  Rec_data <- model_result[["report"]][["Rec"]]

  # Ensure age_at_recruitment does not exceed data length
  if (age_at_recruitment >= length(SSB_data)) {
    stop("age_at_recruitment is too large compared to data length.")
  }
  
  # Adjust SSB to align with Rec based on recruitment age
  SSB_adjusted <- SSB_data[1:(length(SSB_data) - age_at_recruitment)]
  Rec_adjusted <- Rec_data[(age_at_recruitment + 1):length(Rec_data)]
  
  # Create a data frame for ggplot
  plot_data <- data.frame(SSB = SSB_adjusted, Rec = Rec_adjusted)

  # Create the plot
  p <-  ggplot2::ggplot(plot_data, aes(x = SSB, y = Rec)) +
    ggplot2::geom_point(size= point_size, color= point_color,shape=point_shape) +
    ggplot2:: labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "ssb"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "rec"), title = if (!is.null(title)) title else .acl_title("SSB_Rec"))+
    .acl_scale_x(x_breaks, n_breaks = 8) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

  data_out <-plot_data

  if (return_data) {
    return(list(plot = p, data = data_out))
  } else {
    return(p)
  }
}




