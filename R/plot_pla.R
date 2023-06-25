#' Plot probability length at age
#'
#' @param model_result A list that contains model output.
#' @param low_col color for low probabilities
#' @param high_col color for high probabilities
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_prob_heatmap(model_result, "white", "steelblue")
#' }
plot_pla <- function(model_result, low_col = "white", high_col = "red") {
  require(ggplot2)
  require(reshape2)

  pla=model_result[["report"]][["pla"]]
  #
  df <- as.data.frame(pla)

  #
  colnames(df) <- paste0("Age", 1:ncol(df))
  df$Length <- paste("Length",1:nrow(df), sep="")

  #
  dfm <- melt(df, id.vars = "Length")

  #
  p <- ggplot(dfm, aes(x=variable, y=Length, fill=value)) +
    geom_tile() +
    scale_fill_gradient(low = low_col, high = high_col) +
    theme_minimal() +
    labs(x = "Age Group", y = "Length Group", fill = "Probability")

  return(p)
}
