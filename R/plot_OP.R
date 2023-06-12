#' Plot OP over the years
#'
#' This function takes a list and a data frame as input, and plots Elog_index (counts in length groups) over the years using ggplot2. The plot is faceted by the length groups.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "Elog_index" component representing counts in length groups.
#' @param data.CatL A data frame that contains a "Year" column and multiple length group columns.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.5.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param point_color Character. The color of the point in the plot. Default is "black".
#' @param point_size Character. The size of the point in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param point_shape Character. The shape of the point in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param facet_ncol Integer, number of columns in facet_wrap.
#' @param facet_scales Character, scales for facet_wrap.
#' @export
#' @examples
#' \dontrun{
#' plot_OP(model_result, data.CatL)
#' }
plot_OP <- function(model_result, data.CatL,point_size=1,point_color="black",point_shape=1 ,line_size = 1.2, line_color = "black", line_type = "solid",facet_ncol = 3, facet_scales = "free"){

  # Transform the logN_at_len data

  logN_at_len <- as.matrix(log(data.CatL[, 2:ncol(data.CatL)]+1e-5 ))

  # Extract the Elog_index data
  Elog_index <- model_result[["report"]][["Elog_index"]]

  # Make sure it's a matrix
  if(!is.matrix(Elog_index)){
    Elog_index <- as.matrix(Elog_index)
  }

  # Create Year variable from column names of Elog_index (assuming columns are years)
  cl_l <- tidyr::gather(data.CatL,key="Year",value="length",2:ncol(data.CatL))
  Year <- cl_l %>%
    dplyr::mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    dplyr::distinct(Year) %>%
    dplyr::pull(Year)

  # Create LengthGroup variable from row names of Elog_index
  LengthGroup <- paste0("Length bin ", seq_len(nrow(Elog_index)))

  # Convert matrix to data frame in long format
  Elog_index_long <- reshape2::melt(Elog_index)
  colnames(Elog_index_long) <- c("LengthGroup", "Year", "Count")
  Elog_index_long$Year <- Year[match(Elog_index_long$Year, 1:length(Year))]
  Elog_index_long$LengthGroup <- LengthGroup[as.numeric(Elog_index_long$LengthGroup)]



  # Convert matrix to data frame in long format
  logN_at_len_long <- reshape2::melt(logN_at_len)
  colnames(logN_at_len_long) <- c("LengthGroup", "Year", "Count")

  logN_at_len_long$Year <- logN_at_len_long %>%
    dplyr::mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    dplyr::pull(Year)


  logN_at_len_long$LengthGroup <- LengthGroup[as.numeric(logN_at_len_long$LengthGroup)]

  # Plot Elog_index over the years using ggplot2
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(logN_at_len_long, mapping=aes(x = Year, y = Count),size= point_size, color= point_color,shape=point_shape) +
    ggplot2::geom_line(Elog_index_long,  mapping=aes(x = Year, y = Count), size = line_size, color = line_color, linetype = line_type) +
    ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
    ggplot2::labs(x = "Year", y = "Abundance", title = "Elog_index Over Years") +
    ggplot2::theme_minimal()

  return(p)
}
