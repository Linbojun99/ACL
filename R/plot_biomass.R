#' Plot biomass over the years
#'
#' This function takes a list and a data frame as input, and plots biomass over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "B" component representing biomass.
#' @param data.CatL A data frame that contains a "Year" column.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.5.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' plot_biomass(model_result, data.CatL, line_size = 1.2, line_color = "red", line_type = "solid")
#' }
plot_biomass <- function(model_result, data.CatL, line_size = 1.2, line_color = "black", line_type = "solid"){
  # Extract the biomass data
  biomass <- model_result[["report"]][["B"]]

  # Make sure it's a data frame
  if(!is.data.frame(biomass)){
    biomass <- as.data.frame(biomass)
  }

  # Add Year to the biomass data
  cl_l <- tidyr::gather(data.CatL,key="Year",value="length",2:ncol(data.CatL))
  biomass$Year <- cl_l %>%
    dplyr::mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    dplyr::distinct(Year) %>%
    dplyr::pull(Year)

  # Plot biomass over the years using ggplot2
  p <- ggplot2::ggplot(biomass, aes(x = Year, y = biomass)) +
    ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
    ggplot2::labs(x = "Year", y = "Biomass", title = "Biomass Over Years") +
    ggplot2::theme_minimal()

  return(p)
}

