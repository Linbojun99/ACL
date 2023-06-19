#' Plot BL over the years
#'
#' This function takes a list and a data frame as input, and plots BL (counts in length groups) over the years using ggplot2. The plot is faceted by the length groups.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "BL" component representing counts in length groups.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.5.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param facet_ncol Integer, number of columns in facet_wrap.
#' @param facet_scales Character, scales for facet_wrap.
#' @export
#' @examples
#' \dontrun{
#' plot_BL(model_result)
#' }
plot_BL <- function(model_result, line_size = 1.2, line_color = "black", line_type = "solid",facet_ncol = 3, facet_scales = "free"){

  # Extract the BL data
  BL <- model_result[["report"]][["BL"]]

  # Make sure it's a matrix
  if(!is.matrix(BL)){
    BL <- as.matrix(BL)
  }

  # Create Year variable from column names of BL (assuming columns are years)
  Year <- model_result[["year"]]

  # Create LengthGroup variable from row names of BL
  LengthGroup <- paste0("Length bin ", seq_len(nrow(BL)))
  LengthGroup <- factor(paste("Length bin ", seq_len(nrow(BL))), levels=paste("Length bin ", seq_len(nrow(BL))))

  # Convert matrix to data frame in long format
  BL_long <- reshape2::melt(BL)
  colnames(BL_long) <- c("LengthGroup", "Year", "Count")
  BL_long$Year <- Year[match(BL_long$Year, 1:length(Year))]
  BL_long$LengthGroup <- LengthGroup[as.numeric(BL_long$LengthGroup)]




  # Plot BL over the years using ggplot2
  p <- ggplot2::ggplot(BL_long, aes(x = Year, y = Count)) +
    ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
    ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
    ggplot2::labs(x = "Year", y = "Relative abundance", title = "BL Over Years") +
    ggplot2::theme_minimal()

  return(p)
}
