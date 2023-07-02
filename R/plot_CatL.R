#' Plot Estimated and Observed Catch-at-Length (Log or not) over the years or across lengths
#'
#' This function visualizes the Elog_index (counts in length groups) over years or across lengths using ggplot2.
#' The Elog_index represents the estimated abundance of the population within certain length groups, obtained from the model output.
#' The resulting plot is a line graph, with data points represented by distinct markers.
#' The function allows for faceted plots to better visualize the distribution of counts across different length groups or years.
#'
#' @param model_result A list that contains the model output. The list should have a "report" component which contains an "Elog_index" component representing counts in length groups.
#' @param line_size Numeric. Specifies the thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. Specifies the color of the line in the plot. Default is "black".
#' @param line_color1 Character. The color of the line for estimated abundance (Elog_index). Default is "red".
#' @param line_color2 Character. The color of the line for observed abundance (logN_at_len). Default is "blue".
#' @param line_type Character. Specifies the type of the line in the plot. Default is "solid".
#' @param point_color Character. Specifies the color of the point in the plot. Default is "black".
#' @param point_size Numeric. Specifies the size of the point in the plot. Default is 1.
#' @param point_shape Numeric. Specifies the shape of the point in the plot. Default is 1.
#' @param facet_ncol Integer. Specifies the number of columns in the facet_wrap. Default is NULL.
#' @param facet_scales Character. Specifies scales for facet_wrap. Default is "free".
#' @param type Character. It specifies whether the Elog_index is plotted across "length" or "year". Default is "length".
#' @param exp_transform Logical. Specifies whether to apply the exponential function to the data before plotting. If TRUE, the exponential of the data values is plotted. Default is FALSE.
#'
#' @return A ggplot object representing the plot.
#'
#' @examples
#' \dontrun{
#' # Use 'run_acl' to obtain 'model_result'
#' model_result <- run_acl(...)
#'
#' # Plot Elog_index across lengths
#' plot_CatL(model_result, type = "length")
#'
#' # Plot Elog_index over years
#' plot_CatL(model_result, type = "year")
#'
#' # With exp_transform set to TRUE
#' plot_CatL(model_result, type = "...", exp_transform = TRUE)
#' }
#'
#' @export

plot_CatL <- function(model_result, point_size=1, point_color="black", point_shape=1, line_size = 1.2, line_color = "black", line_type = "solid", line_color1 = "red", line_color2 = "blue", facet_ncol = NULL, facet_scales = "free", type = c("length","year"), exp_transform = FALSE){


  # Apply exp transformation if exp_transform is TRUE
    transform_fn <- if(exp_transform) exp else identity


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


  # Just before the plotting, apply the transformation function to 'Count' column
  Elog_index_long$Count <- transform_fn(Elog_index_long$Count)
  logN_at_len_long$Count <- transform_fn(logN_at_len_long$Count)

  # Plot Elog_index over the years using ggplot2
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(logN_at_len_long, mapping=aes(x = Year, y = Count),size= point_size, color= point_color,shape=point_shape) +
    ggplot2::geom_line(Elog_index_long,  mapping=aes(x = Year, y = Count), size = line_size, color = line_color, linetype = line_type) +
    ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
    ggplot2::labs(x = "Year", y = "Abundance", title = "Estimated(Line) and Observed(Point) Catch-at-Length Over Years") +
    ggplot2::theme_minimal()
  }

    if(type=="year"){
      # Define the data
      Elog_index <- model_result[["report"]][["Elog_index"]]
      logN_at_len<-model_result[["obj"]][["env"]][[".data"]][["logN_at_len"]]
      len_mid <- model_result[["len_mid"]]
      year <- model_result[["year"]]

      # Transform the matrix into a data frame and add column names
      df1 <- as.data.frame(Elog_index)
      colnames(df1) <- year
      df1$len_mid <- len_mid

      df2 <- as.data.frame(logN_at_len)
      colnames(df2) <- year
      df2$len_mid <- len_mid

      # Reshape the data from wide to long format
      df_long1 <- df1 %>%
        tidyr::pivot_longer(-len_mid, names_to = "year", values_to = "Abundance")

      df_long1$Abundance <- transform_fn(df_long1$Abundance)

      df_long2 <- df2 %>%
        tidyr::pivot_longer(-len_mid, names_to = "year", values_to = "Abundance")

      df_long2$Abundance <- transform_fn(df_long2$Abundance)

      # Plot the data using ggplot2
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_long1, mapping=aes(x = len_mid, y = Abundance),size=line_size,color=line_color1,linetype=line_type) +
        ggplot2::geom_line(data=df_long2, mapping=aes(x = len_mid, y = Abundance),size=line_size,color=line_color2,linetype=line_type) +
        ggplot2::facet_wrap(~year, scales = facet_scales,ncol=facet_ncol) +
        ggplot2::labs(x = "Body Length", y = "Abundance", title = "Estimated(Red) and Observed(Blue) Catch-at-Length Distribution Yearly") +
        ggplot2::theme_minimal()

      return(p)
    }
}
