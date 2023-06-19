#' Plot SSB and Rec from model results
#'
#' This function takes the model result, extracts the SSB and Rec components, and plots them.
#'
#' @param model_result A list containing model results, including "report" that has "SSB" and "Rec".
#' @param point_color Character. The color of the point in the plot. Default is "black".
#' @param point_size Character. The size of the point in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @param point_shape Character. The shape of the point in the plot. Default is "solid".#' @return A ggplot object representing the plot.
#' @return ggplot object
#' @examples
#' \dontrun{
#' plot_SSB_Rec(model_result)
#' }
#' @export

plot_SSB_Rec <- function(model_result,point_size=2,point_color="black",point_shape=16) {


  # Extract the necessary data
  SSB_data <- model_result[["report"]][["SSB"]]
  Rec_data <- model_result[["report"]][["Rec"]]

  # Create a data frame for ggplot
  plot_data <- data.frame(SSB = SSB_data, Rec = Rec_data)

  # Create the plot
  plot <-  ggplot2::ggplot(plot_data, aes(x = SSB, y = Rec)) +
    ggplot2::geom_point(size= point_size, color= point_color,shape=point_shape) +
    ggplot2:: labs(x = "SSB", y = "Rec")+
    ggplot2::theme_minimal()

  return(plot)
}
