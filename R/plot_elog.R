#' Plot OP over the years
#'
#' This function takes a list and a data frame as input, and plots Elog_index (counts in length groups) over the years using ggplot2. The plot is faceted by the length groups.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "Elog_index" component representing counts in length groups.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.5.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param point_color Character. The color of the point in the plot. Default is "black".
#' @param point_size Character. The size of the point in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param point_shape Character. The shape of the point in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param facet_ncol Integer, number of columns in facet_wrap.
#' @param facet_scales Character, scales for facet_wrap.
#' @param type Character. It specifies whether the elogindex are calculated for "length" or "year". Default is "length".
#' @export
#' @examples
#' \dontrun{
#' plot_elog(model_result)
#' }
plot_elog <- function(model_result,point_size=1,point_color="black",point_shape=1 ,line_size = 1.2, line_color = "black", line_type = "solid",facet_ncol = 3, facet_scales = "free",type=c("length","year")){
  if(type=="length"){
  # Transform the logN_at_len data

  logN_at_len<-model_result[["obj"]][["env"]][[".data"]][["logN_at_len"]]
  # Extract the Elog_index data
  Elog_index <- model_result[["report"]][["Elog_index"]]

  # Make sure it's a matrix
  if(!is.matrix(Elog_index)){
    Elog_index <- as.matrix(Elog_index)
  }

  # Create Year variable from column names of Elog_index (assuming columns are years)

    Year <-model_result[["year"]]


  # Create LengthGroup variable from row names of Elog_index
  LengthGroup <- paste0("Length bin ", seq_len(nrow(Elog_index)))
  LengthGroup <- factor(paste("Length bin ", seq_len(nrow(Elog_index))), levels=paste("Length bin ", seq_len(nrow(Elog_index))))

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
  }
  if(type=="year"){
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
      ggplot2::labs(x = "Body Length", y = "Abundance", title = "Elog_index Length Distribution Yearly") +
      ggplot2::theme_minimal()

  }
  return(p)
}
