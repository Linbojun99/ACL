#' Visualize the Probability Length at Age in Fisheries Stock Assessment through Heatmap
#'
#' This function plays an essential part in the process of Annual Catch Limit (ACL) in Fisheries Stock Assessment.
#' It generates a heatmap to visualize the Probability Length at Age (PLA) derived from a given model result.
#' The PLA matrix is an essential output from fish stock assessment models which shows the probability that a fish of a certain age is within a certain length group.
#' This visualization is beneficial to understand the size structure of the fish population over age, which is crucial for setting appropriate catch limits.
#' The function also allows customization of the color gradient used in the heatmap to make the contrast between different probability values more apparent.
#'
#' @param model_result A list that contains model output. The model output should include a PLA report.
#' @param low_col A string specifying the color for low probabilities. Default is "white".
#' @param high_col A string specifying the color for high probabilities. Default is "red".
#'
#' @return A ggplot object showing the heatmap of the probability length at age.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume we have a model_result list from a fishery stock assessment model
#' model_result <- run_model()
#'
#' # Generate a heatmap with the default color gradient
#' plot_pla(model_result)
#'
#' # Generate a heatmap with a custom color gradient from white to steelblue
#' plot_pla(model_result, "white", "steelblue")
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
