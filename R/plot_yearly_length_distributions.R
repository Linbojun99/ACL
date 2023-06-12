#' Plot yearly length distributions
#'
#' This function takes a model_result list as input, extracts the Elog_index, len_mid, and year,
#' and then plots the yearly length distributions using ggplot2.
#'
#' @param model_result A list that contains model output.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.5.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param point_color Character. The color of the point in the plot. Default is "black".
#' @param point_size Character. The size of the point in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param point_shape Character. The shape of the point in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param facet_ncol Integer, number of columns in facet_wrap.
#' @param facet_scales Character, scales for facet_wrap.
#' The list should have a "report" component which contains "Elog_index" component,
#' and also "len_mid" and "year" components.
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' plot_yearly_length_distributions(model_result)
#' }
plot_yearly_length_distributions <- function(model_result,point_size=1.5,point_color="black",point_shape=1 ,line_size = 1.2, line_color = "black", line_type = "solid",facet_ncol = 5, facet_scales = "fixed"){
  # Define the data
  Elog_index <- model_result[["report"]][["Elog_index"]]
  len_mid <- model_result[["len_mid"]]
  year <- model_result[["year"]]

  # Transform the matrix into a data frame and add column names
  df <- as.data.frame(Elog_index)
  colnames(df) <- year
  df$len_mid <- len_mid

  # Reshape the data from wide to long format
  df_long <- df %>%
    tidyr::pivot_longer(-len_mid, names_to = "year", values_to = "Abundance")

  # Plot the data using ggplot2
  p <- ggplot2::ggplot(df_long, aes(x = len_mid, y = Abundance)) +
    ggplot2::geom_line(size=line_size,color=line_color,linetype=line_type) +
    ggplot2::geom_point(size=point_size,color=point_color,shape=point_shape) +
    ggplot2::facet_wrap(~year, scales = facet_scales,ncol=facet_ncol) +
    ggplot2::labs(x = "Body Length", y = "Abundance", title = "Yearly Length Distribution") +
    ggplot2::theme_minimal()

  return(p)
}
