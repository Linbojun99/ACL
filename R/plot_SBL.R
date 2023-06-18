#' Plot SBL over the years
#'
#' This function takes a list and a data frame as input, and plots SBL (counts in length groups) over the years using ggplot2. The plot is faceted by the length groups.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "SBL" component representing counts in length groups.
#' @param data.CatL A data frame that contains a "Year" column and multiple length group columns.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.5.
#' @param line_color Character. The color of the line in the plot. Default is "SBLack".
#' @param line_type Character. The type of the line in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param facet_ncol Integer, number of columns in facet_wrap.
#' @param facet_scales Character, scales for facet_wrap.
#' @export
#' @examples
#' \dontrun{
#' plot_SBL(model_result, data.CatL)
#' }
plot_SBL <- function(model_result, data.CatL, line_size = 1.2, line_color = "bLack", line_type = "solid",facet_ncol = 3, facet_scales = "free"){

  # Extract the SBL data
  SBL <- model_result[["report"]][["SBL"]]

  # Make sure it's a matrix
  if(!is.matrix(SBL)){
    SBL <- as.matrix(SBL)
  }

  # Create Year variable from column names of SBL (assuming columns are years)
  colnames(SBL) <- as.character(model_result[["year"]])
  Year <- model_result[["year"]]

  # Create LengthGroup variable from row names of SBL
  LengthGroup <- paste0("Length bin ", seq_len(nrow(SBL)))


  # Convert matrix to data frame in long format
  SBL_long <- reshape2::melt(SBL)
  colnames(SBL_long) <- c("LengthGroup", "Year", "Count")
  SBL_long$LengthGroup <- LengthGroup[as.numeric(SBL_long$LengthGroup)]

  # Plot SBL over the years using ggplot2
  p <- ggplot2::ggplot(SBL_long, aes(x = Year, y = Count)) +
    ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
    ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
    ggplot2::labs(x = "Year", y = "Relative abundance", title = "SBL Over Years") +
    ggplot2::theme_minimal()

  return(p)
}
