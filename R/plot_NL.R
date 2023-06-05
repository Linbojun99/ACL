#' Plot NL over the years
#'
#' This function takes a list and a data frame as input, and plots NL (counts in length groups) over the years using ggplot2. The plot is faceted by the length groups.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "NL" component representing counts in length groups.
#' @param data.CatL A data frame that contains a "Year" column and multiple length group columns.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.5.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param n_col Integer, number of columns in facet_wrap.
#' @param scales Character, scales for facet_wrap.
#' @export
#' @examples
#' \dontrun{
#' plot_NL(model_result, data.CatL)
#' }
plot_NL <- function(model_result, data.CatL, line_size = 1.2, line_color = "black", line_type = "solid",n_col = 3, scales = "free"){

  # Extract the NL data
  NL <- model_result[["report"]][["NL"]]

  # Make sure it's a matrix
  if(!is.matrix(NL)){
    NL <- as.matrix(NL)
  }

  # Create Year variable from column names of NL (assuming columns are years)
  cl_l <- tidyr::gather(data.CatL,key="Year",value="length",2:ncol(data.CatL))
  Year <- cl_l %>%
    dplyr::mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    dplyr::distinct(Year) %>%
    dplyr::pull(Year)

  # Create LengthGroup variable from row names of NL
  LengthGroup <- paste0("Length bin ", seq_len(nrow(NL)))

  # Convert matrix to data frame in long format
  NL_long <- reshape2::melt(NL)
  colnames(NL_long) <- c("LengthGroup", "Year", "Count")
  NL_long$Year <- Year[match(NL_long$Year, 1:length(Year))]
  NL_long$LengthGroup <- LengthGroup[as.numeric(NL_long$LengthGroup)]

  # Plot NL over the years using ggplot2
  p <- ggplot2::ggplot(NL_long, aes(x = Year, y = Count)) +
    ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
    ggplot2::facet_wrap(~LengthGroup, ncol = n_col, scales = scales) +
    ggplot2::labs(x = "Year", y = "Count", title = "NL Over Years") +
    ggplot2::theme_minimal()

  return(p)
}
