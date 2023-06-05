#' Plot fishing mortality over the years
#'
#' This function takes a matrix and a data frame as input, and plots fishing mortality over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "BL" component representing counts in length groups.
#' @param data.CatL A data frame that contains a "Year" column.
#' @param line_size Line size for geom_line. Default is 1.
#' @param line_color Line color for geom_line. Default is "black".
#' @param line_type Line type for geom_line. Default is "solid".
#' @param facet_ncol Number of columns in facet wrap. Default is 3.
#' @param facet_scales Scales for facet wrap. Default is "free".
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' plot_fishing_mortality(F, data.cl)
#' }
plot_fishing_mortality <- function(model_result,data.CatL, line_size = 1, line_color = "black", line_type = "solid", facet_ncol = 3, facet_scales = "free"){

  # Extract the F data
  F<- model_result[["report"]][["F"]]

  # Create Year variable from column names of F (assuming columns are years)
  cl_l <- tidyr::gather(data.CatL,key="Year",value="length",2:ncol(data.CatL))
  Year <- cl_l %>%
    dplyr::mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    dplyr::distinct(Year) %>%
    dplyr::pull(Year)


  # Create AgeGroup variable from row names of F
  AgeGroup <- paste0("Age group ", seq_len(nrow(F)))

  # Convert matrix to data frame in long format
  F_long <- reshape2::melt(F)
  colnames(F_long) <- c("AgeGroup", "Year", "F")
  F_long$Year <- Year[as.numeric(F_long$Year)]
  F_long$AgeGroup <- AgeGroup[as.numeric(F_long$AgeGroup)]

  # Plot fishing mortality over the years using ggplot2
  p <- ggplot2::ggplot(F_long, aes(x = Year, y = F)) +
    ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
    ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
    ggplot2::labs(x = "Year", y = "Fishing Mortality", title = "Fishing Mortality Over Years") +
    ggplot2::theme_minimal()

  return(p)
}
