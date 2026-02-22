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
#' @param title Character or NULL. Custom plot title. If NULL, uses global theme setting. See \code{acl_theme_set()}.
#' @param xlab Character or NULL. Custom x-axis label. If NULL, uses global theme setting.
#' @param ylab Character or NULL. Custom y-axis label. If NULL, uses global theme setting.
#' @param font_family Character or NULL. Custom font family. If NULL, uses global theme setting (default "Arial").
#' @param title_size Numeric or NULL. Plot title size in pt. If NULL, uses global theme (default 14).
#' @param axis_title_size Numeric or NULL. Axis title size in pt. If NULL, uses global theme (default 12).
#' @param axis_text_size Numeric or NULL. Axis tick label size in pt. If NULL, uses global theme (default 10).
#' @param strip_text_size Numeric or NULL. Facet label size in pt. If NULL, uses global theme (default 10).
#' @param legend_text_size Numeric or NULL. Legend text size in pt. If NULL, uses global theme (default 10).
#' @param x_breaks Numeric vector or NULL. Custom x-axis breaks (e.g. \code{seq(1, 20, by = 2)}). NULL = auto.
#' @param base_theme Character or NULL. Base ggplot2 theme name (e.g. "theme_bw"). NULL = global setting.
#' @param title_hjust Numeric or NULL. Title horizontal alignment: 0 = left, 0.5 = center, 1 = right. NULL = global setting.
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
plot_pla <- function(model_result, low_col = "white", high_col = "red", title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL) {

  pla=model_result[["report"]][["pla"]]
  len_label=model_result[["len_label"]]

  df <- as.data.frame(pla)

  age_labels <- paste("Age bin", seq_len(ncol(df)))
  colnames(df) <- age_labels

  len_levels <- .acl_fix_len_labels(len_label)
  df$LengthGroup <- factor(len_levels, levels = len_levels)

  dfm <- reshape2::melt(df, id.vars = "LengthGroup")
  dfm$variable <- factor(dfm$variable, levels = age_labels)

  #
  p <- ggplot2::ggplot(dfm, ggplot2::aes(x=variable, y=LengthGroup, fill=value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = low_col, high = high_col) +
    .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust) +
    ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "age"), y = if (!is.null(ylab)) ylab else .acl_lab("x", "length"), fill = "Probability", title = if (!is.null(title)) title else .acl_title("pla"))

  return(p)
}
