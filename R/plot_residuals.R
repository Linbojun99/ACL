#' Plot model residuals with facets
#'
#' This function creates a facet plot of the residuals for each index in the model result. Each index is represented in a separate facet.
#'
#' @param model_result A list that contains the model output. The list should have a "report" component which contains a "resid_index" component representing the residuals for each index.
#' @param f Numeric. The smoother span. This gives the proportion of points in the plot which influence the smooth at each value. Larger values give more smoothness. Default is 0.4.
#' @param line_color Character. The color of the lines in the plot. Default is "black".
#' @param smooth_color Character. The color of the smooth line in the plot. Default is "blue".
#' @param hline_color Character. The color of the horizontal line in the plot. Default is "red".
#' @param line_size Numeric. The size of the lines in the plot. Default is 1.
#' @param facet_scales Character. The scale argument for facet_wrap. Default is "free".
#' @param type Character. It specifies whether the residuals are calculated for "length" or "age". Default is "length".
#' @return A ggplot object representing the facet plot.
#' @export
#' @examples
#' \dontrun{
#' plot_residuals(model_result)
#' }
plot_residuals <- function(model_result, f = 0.4, line_color = "black", smooth_color = "blue", hline_color = "red", line_size = 1, facet_scales = "free", type=c("length","age")) {
  if(type=="length"){

    num_indices <- dim(model_result[["report"]][["resid_index"]])[1]
    plot_data <- data.frame()
    for(i in seq_len(num_indices)){
      temp_data <- as.data.frame(model_result[["report"]][["resid_index"]][i,])
      temp_data$LengthGroup <- factor(paste("Length bin ", i), levels=paste("Length bin ", 1:num_indices))
      temp_data$age <- 1:nrow(temp_data)
      plot_data <- rbind(plot_data, temp_data)
    }
    colnames(plot_data) <- c("residual", "LengthGroup","age")

    p <- ggplot(plot_data, aes(x=age, y=residual)) +
      geom_line(color = line_color, size = line_size) +
      geom_smooth(method="loess", formula=y~x, se=FALSE, color=smooth_color, linetype=2, size=line_size) +
      geom_hline(yintercept=0, color=hline_color, size=line_size) +
      facet_wrap(~LengthGroup, scales = facet_scales)+
      theme_minimal()
  }
  if (type=="age")
  {
    num_indices <-dim(model_result[["report"]][["resid_index"]])[2]
    plot_data <- data.frame()
    for(i in seq_len(num_indices)){
      temp_data <- as.data.frame(model_result[["report"]][["resid_index"]][,i])
      temp_data$AgeGroup <- factor(paste("Age bin ", i), levels=paste("Age bin ", 1:num_indices))
      temp_data$length <- 1:nrow(temp_data)
      plot_data <- rbind(plot_data, temp_data)
    }
    colnames(plot_data) <- c("residual", "AgeGroup","length")

    p <- ggplot(plot_data, aes(x=length, y=residual)) +
      geom_line(color = line_color, size = line_size) +
      geom_smooth(method="loess", formula=y~x, se=FALSE, color=smooth_color, linetype=2, size=line_size) +
      geom_hline(yintercept=0, color=hline_color, size=line_size) +
      facet_wrap(~AgeGroup, scales = facet_scales)+
      theme_minimal()
  }
  return(p)
}
