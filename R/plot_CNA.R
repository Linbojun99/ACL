#' Plot catch numbers by age over the years
#'
#' This function takes a matrix and a data frame as input, and plots fishing mortality over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "BL" component representing counts in length groups.
#' @param line_size Line size for geom_line. Default is 1.
#' @param line_color Line color for geom_line. Default is "black".
#' @param line_type Line type for geom_line. Default is "solid".
#' @param facet_ncol Number of columns in facet wrap. Default is 3.
#' @param facet_scales Scales for facet wrap. Default is "free".
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' plot_CNA(a)
#' }
plot_CNA <- function(model_result, line_size = 1, line_color = "red", line_type = "solid", facet_ncol = 3, facet_scales = "free" ){

  # Extract the CNA data
  CNA<- model_result[["report"]][["CNA"]]

  # Create Year variable from column names of CNA (assuming columns are years)
  Year <- model_result[["year"]]

  # Create AgeGroup variable from row names of CNA
  AgeGroup <- paste0("Age group ", seq_len(nrow(CNA)))
  AgeGroup <- factor(paste("Age bin ", seq_len(nrow(CNA))), levels=paste("Age bin ",seq_len(nrow(CNA))))

  # Convert matrix to data frame in long format
  CNA_long <- reshape2::melt(CNA)
  colnames(CNA_long) <- c("AgeGroup", "Year", "CNA")
  CNA_long$Year <- Year[as.numeric(CNA_long$Year)]
  CNA_long$AgeGroup <- AgeGroup[as.numeric(CNA_long$AgeGroup)]


    # Plot catch numbers by age over the years using ggplot2
    p <- ggplot2::ggplot(CNA_long, aes(x = Year, y = CNA)) +
      ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
      ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
      ggplot2::labs(x = "Year", y = "Catch numbers by age", title = "Catch numbers by age Over Years") +
      ggplot2::theme_minimal()



  return(p)
}
