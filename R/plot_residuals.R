#' @title Plot the Distribution of Model Residuals from an Age-Structured Assessment Model (ACL)
#'
#' @description This function takes the output from the `run_acl` function and creates a facet plot of the residuals for each index.
#' The residuals reflect the difference between the observed and expected values from the ACL model, a type of age-structured
#' assessment model commonly used in fishery science. The function can handle two types of residuals:
#' 1) "length", where residuals are calculated over length groups for each year, and
#' 2) "year", where residuals are calculated over years for each length group.
#' Each index is represented in a separate facet.
#'
#' @param model_result A list obtained from the `run_acl` function. This list should contain a "report" component that includes
#' a "resid_index" component, representing the residuals for each index.
#' @param f Numeric. The smoother span for the loess smooth line in the plot. This gives the proportion of points in the plot which
#' influence the smooth at each value. Larger values result in more smoothing. Default is 0.4.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param smooth_color Character. The color of the smooth line in the plot. Default is "blue".
#' @param hline_color Character. The color of the horizontal line (at y=0) in the plot. Default is "red".
#' @param line_size Numeric. The size of the lines in the plot. Default is 1.
#' @param facet_scales Character. The "scales" argument for the facet_wrap function in ggplot2. Default is "free".
#' @param type Character. Specifies whether the residuals are calculated over "length" or "year". Default is "length".
#'
#' @return A ggplot object representing the facet plot of residuals.
#'
#' @examples
#' \dontrun{
#' # Use 'run_acl' to get 'model_result'
#' model_result <- run_acl(...)
#'
#' # Call the function with type = "length"
#' p_length <- plot_residuals(model_result, type = "length")
#'
#' # Print the plot for "length"
#' print(p_length)
#'
#' # Call the function with type = "year"
#' p_year <- plot_residuals(model_result, type = "year")
#'
#' # Print the plot for "year"
#' print(p_year)
#' }
#' @export
plot_residuals <- function(model_result, f = 0.4, line_color = "black", smooth_color = "blue", hline_color = "red", line_size = 1, facet_scales = "free", type=c("length","year")) {
  if(type=="length"){

    num_indices <- dim(model_result[["report"]][["resid_index"]])[1]
    plot_data <- data.frame()
    for(i in seq_len(num_indices)){
      temp_data <- as.data.frame(model_result[["report"]][["resid_index"]][i,])
      temp_data$LengthGroup <- factor(paste("Length bin ", i), levels=paste("Length bin ", 1:num_indices))
      temp_data$year <- model_result[["year"]]
      plot_data <- rbind(plot_data, temp_data)
    }
    colnames(plot_data) <- c("residual", "LengthGroup","year")

    p <- ggplot(plot_data, aes(x=year, y=residual)) +
      geom_line(color = line_color, size = line_size) +
      geom_smooth(method="loess", formula=y~x, se=FALSE, color=smooth_color, linetype=2, size=line_size) +
      geom_hline(yintercept=0, color=hline_color, size=line_size) +
      facet_wrap(~LengthGroup, scales = facet_scales)+
      theme_minimal()
  }
  if (type=="year")
  {

    # Define the data
    resid_index <- model_result[["report"]][["resid_index"]]
    year <- model_result[["year"]]
    len_mid <- model_result[["len_mid"]]

    # Transform the matrix into a data frame and add column names
    df <- as.data.frame(resid_index)
    colnames(df) <- year
    df$len_mid <- len_mid

    # Reshape the data from wide to long format
    df_long <- df %>%
      tidyr::pivot_longer(-len_mid, names_to = "year", values_to = "residuals")


    colnames(df_long) <- c("length", "Year","residual")

    p <- ggplot(df_long, aes(x=length, y=residual)) +
      geom_line(color = line_color, size = line_size) +
      geom_smooth(method="loess", formula=y~x, se=FALSE, color=smooth_color, linetype=2, size=line_size) +
      geom_hline(yintercept=0, color=hline_color, size=line_size) +
      facet_wrap(~Year, scales = facet_scales)+
      theme_minimal()
  }
  return(p)
}
