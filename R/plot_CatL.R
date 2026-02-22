#' Plot Estimated and Observed Catch-at-Length (Log or not) over the years or across lengths
#'
#' This function visualizes the Elog_index (counts in length groups) over years or across lengths using ggplot2.
#' The Elog_index represents the estimated abundance of the population within certain length groups, obtained from the model output.
#' The resulting plot is a line graph, with data points represented by distinct markers.
#' The function allows for faceted plots to better visualize the distribution of counts across different length groups or years.
#'
#' @param model_result A list that contains the model output. The list should have a "report" component which contains an "Elog_index" component representing counts in length groups.
#' @param line_size Numeric. Line thickness for type="length" plot. Default is 1.2.
#' @param line_color Character. Line color for type="length" plot. Default is "black".
#' @param line_type Character. Line type for type="length" plot. Default is "solid".
#' @param line_size1 Numeric. Line thickness for estimated (Elog_index) in type="year". Default is 1.8.
#' @param line_color1 Character. Line color for estimated abundance. Default is "#D32F2F".
#' @param line_type1 Character. Line type for estimated line. Default is "solid".
#' @param line_alpha1 Numeric. Transparency for estimated line (0-1). Default is 1.
#' @param line_size2 Numeric. Line thickness for observed (logN_at_len) in type="year". Default is 1.0.
#' @param line_color2 Character. Line color for observed abundance. Default is "#1976D2".
#' @param line_type2 Character. Line type for observed line. Default is "solid".
#' @param line_alpha2 Numeric. Transparency for observed line (0-1). Default is 0.6.
#' @param point_color Character. Specifies the color of the point in the plot. Default is "#D32F2F".
#' @param point_size Numeric. Specifies the size of the point in the plot. Default is 2.5.
#' @param point_shape Numeric. Specifies the shape of the point in the plot. Default is 16 (filled circle). Common: 1=open circle, 16=filled circle, 17=triangle, 15=square.
#' @param facet_ncol Integer. Specifies the number of columns in the facet_wrap. Default is NULL.
#' @param facet_scales Character. Specifies scales for facet_wrap. Default is "free".
#' @param type Character. It specifies whether the Elog_index is plotted across "length" or "year". Default is "length".
#' @param exp_transform Logical. Specifies whether to apply the exponential function to the data before plotting. If TRUE, the exponential of the data values is plotted. Default is FALSE.
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

plot_CatL <- function(model_result, point_size=2.5, point_color="#D32F2F", point_shape=16,
                      line_size = 1.2, line_color = "black", line_type = "solid",
                      line_size1 = 1.8, line_color1 = "#D32F2F", line_type1 = "solid", line_alpha1 = 1,
                      line_size2 = 1.0, line_color2 = "#1976D2", line_type2 = "solid", line_alpha2 = 0.6,
                      facet_ncol = NULL, facet_scales = "free", type = c("length","year"),
                      exp_transform = FALSE, return_data = FALSE,
                      title = NULL, xlab = NULL, ylab = NULL, font_family = NULL,
                      title_size = NULL, axis_title_size = NULL, axis_text_size = NULL,
                      strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL){


  # Apply exp transformation if exp_transform is TRUE
    transform_fn <- if(exp_transform) exp else identity

    len_label=model_result[["len_label"]]

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
    len_levels <- .acl_fix_len_labels(len_label)

  # Convert matrix to data frame in long format
  Elog_index_long <- reshape2::melt(Elog_index)
  colnames(Elog_index_long) <- c("LengthGroup", "Year", "Count")
  Elog_index_long$Year <- Year[match(Elog_index_long$Year, 1:length(Year))]

    Elog_index_long$LengthGroup <- factor(len_levels[Elog_index_long$LengthGroup],
                                          levels = len_levels)



  # Convert matrix to data frame in long format
  logN_at_len_long <- reshape2::melt(logN_at_len)
  colnames(logN_at_len_long) <- c("LengthGroup", "Year", "Count")

  logN_at_len_long$Year <- logN_at_len_long %>%
    dplyr::mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    dplyr::pull(Year)


    logN_at_len_long$LengthGroup <- factor(len_levels[as.integer(logN_at_len_long$LengthGroup)],
                                           levels = len_levels)


  # Just before the plotting, apply the transformation function to 'Count' column
  Elog_index_long$Count <- transform_fn(Elog_index_long$Count)
  logN_at_len_long$Count <- transform_fn(logN_at_len_long$Count)

  # Plot Elog_index over the years using ggplot2
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(logN_at_len_long, mapping=aes(x = Year, y = Count),size= point_size, color= point_color,shape=point_shape) +
    ggplot2::geom_line(Elog_index_long,  mapping=aes(x = Year, y = Count), linewidth = line_size, color = line_color, linetype = line_type) +
    ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
    ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), title = if (!is.null(title)) title else .acl_title("CatL_year")) +
    .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)


  if (return_data) {
    return(list(plot = p, data1 = Elog_index_long,data2=logN_at_len_long))
  } else {
    return(p)
  }
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
      year_levels <- paste("Year", year)
      df_long1$year <- factor(paste("Year", df_long1$year), levels = year_levels)

      df_long2 <- df2 %>%
        tidyr::pivot_longer(-len_mid, names_to = "year", values_to = "Abundance")

      df_long2$Abundance <- transform_fn(df_long2$Abundance)
      df_long2$year <- factor(paste("Year", df_long2$year), levels = year_levels)

      # Plot the data using ggplot2
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_long2, mapping=aes(x = len_mid, y = Abundance),linewidth=line_size2,color=line_color2,linetype=line_type2,alpha=line_alpha2) +
        ggplot2::geom_line(data=df_long1, mapping=aes(x = len_mid, y = Abundance),linewidth=line_size1,color=line_color1,linetype=line_type1,alpha=line_alpha1) +
        ggplot2::facet_wrap(~year, scales = facet_scales,ncol=facet_ncol) +
        ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "length"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), title = if (!is.null(title)) title else .acl_title("CatL_year_dist")) +
        .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)


      if (return_data) {
        return(list(plot = p, data1 = df_long1, data2=df_long2))
      } else {
        return(p)
      }
    }
}
