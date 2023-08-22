#' @title Plot ridges of Observed and Estimated Catch-at-Length Over Years
#'
#' @description This function takes the model result as an input and creates two ridgeline plots to
#' visualize the density distribution of observed and estimated log catch at length over different years.
#' The density distribution is calculated by transforming the abundance data with the exponential function.
#'
#' @param model_result A list containing the model result, which includes a data frame of Elog_index and logN_at_len, len_mid, and year.
#' @param ridges_alpha Numeric. Specifies the transparency of the ridgeline plots. Default is 0.8.
#'
#' @return A grid arranged ggplot object.
#'
#' @examples
#' \dontrun{
#' # Assume model_result is the result obtained from function 'run_acl'.
#' p <- plot_ridges(model_result)
#' print(p)
#' }
#' @export
plot_ridges <- function(model_result,ridges_alpha=0.8){
  # Define the data
  Elog_index <- model_result[["report"]][["Elog_index"]]
  logN_at_len <- model_result[["obj"]][["env"]][[".data"]][["logN_at_len"]]
  len_mid <- model_result[["len_mid"]]
  year <- model_result[["year"]]

  # Transform the matrices into data frames and add column names
    # Apply the transformation function to 'Abundance' column
  df_observed <- as.data.frame(exp(logN_at_len))
  Total1=as.vector(colSums(df_observed))
  df_observed=as.data.frame(sweep(x=df_observed,MARGIN=2,STATS=Total1,FUN="/"))
  colnames(df_observed) <- year
  df_observed$len_mid <- len_mid

  df_estimated <- as.data.frame(exp(Elog_index))
  Total2=as.vector(colSums(df_estimated))
  df_estimated=as.data.frame(sweep(x=df_estimated,MARGIN=2,STATS=Total2,FUN="/"))
  colnames(df_estimated) <- year
  df_estimated$len_mid <- len_mid

  # Reshape the data from wide to long format
  df_observed_long <- df_observed %>%
    tidyr::pivot_longer(-len_mid, names_to = "year", values_to = "Abundance")

  df_estimated_long <- df_estimated %>%
    tidyr::pivot_longer(-len_mid, names_to = "year", values_to = "Abundance")


  # Plot the data using ggplot2 with ggridges
  p_observed <- ggplot2::ggplot(df_observed_long, aes(x = len_mid, y = as.factor(year), height = Abundance, fill = as.factor(year))) +
    ggridges::geom_density_ridges(stat = "identity", alpha = ridges_alpha) +
    ggplot2::scale_fill_discrete(name = "Year", guide = NULL) +
    ggplot2::labs(x = "Body Length", y = "Abundance", title = "Observed Catch-at-Length Over Years") +
    ggplot2::theme_minimal()

  p_estimated <- ggplot2::ggplot(df_estimated_long, aes(x = len_mid, y = as.factor(year), height = Abundance, fill = as.factor(year))) +
    ggridges::geom_density_ridges(stat = "identity", alpha = ridges_alpha) +
    ggplot2::scale_fill_discrete(name = "Year", guide = NULL) +
    ggplot2::labs(x = "Body Length", y = "Abundance", title = "Estimated Catch-at-Length Over Years") +
    ggplot2::theme_minimal()

  p<- cowplot::plot_grid(p_observed,p_estimated, ncol = 2)

  # Return the plots as a list
  return(p)
}
