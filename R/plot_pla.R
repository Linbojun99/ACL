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

  colnames(df) <- factor(paste("Age bin ", seq_len(ncol(df))), levels=paste("Age bin ",seq_len(ncol(df))))

  LengthGroup <- factor(paste("Length bin ", seq_len(nrow(df))), levels=paste("Length bin ", seq_len(nrow(df))))

  df$LengthGroup <- LengthGroup[as.numeric(LengthGroup)]

  #
  dfm <- melt(df, id.vars = "LengthGroup")

  #
  p <- ggplot(dfm, aes(x=variable, y=LengthGroup, fill=value)) +
    geom_tile() +
    scale_fill_gradient(low = low_col, high = high_col) +
    theme_minimal() +
    labs(x = "Age Group", y = "Length Group", fill = "Probability")

  return(p)
}
