#' Plot Catch Number (CN) or Catch Number by Age (CNA) over the years
#'
#' This function visualizes model outputs in the form of time-series plots of CN or CNA using ggplot2.
#' For CN, it plots a simple time-series. For CNA, it creates a separate time-series plot for each age group.
#'
#' @param model_result A list containing model output. The list should have a "report" component containing either a "CN" or "CNA" component.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. The color of the line in the plot. Default is "red".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @param se_color Character. The color of the confidence interval ribbon. Default is "red".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param facet_ncol Numeric. Number of columns in facet wrap. Default is NULL.
#' @param facet_scales Character. Scales for facet wrap. Default is "free".
#' @param type Character vector. It specifies which type of catch is to be plotted: "CN" or "CNA". Default is c("CN","CNA").
#'
#' @return A ggplot object representing the plot.
#'
#'
#' @examples
#' \dontrun{
#' # Use 'run_acl' to get 'model_result'
#' model_result <- run_acl(...)
#'
#' # Plot CN
#' plot_catch(model_result, type = "CN")
#'
#' # Plot CN with standard error
#' plot_catch(model_result, type = "CN", se = TRUE)
#'
#' # Plot CNA
#' plot_catch(model_result, type = "CNA")
#' }
#'
#' @export
plot_catch <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2,facet_ncol = NULL, facet_scales = "free",type=c("CN","CNA")){
  if(type=="CN")
    {
    # Extract the CN data
  CN <- model_result[["report"]][["CN"]]

  # Make sure it's a data frame
  if(!is.data.frame(CN)){
    CN <- as.data.frame(CN)
  }

  # Add Year to the CN data
  CN$Year <-  model_result[["year"]]

  if(!se)
  {
    # Plot CN over the years using ggplot2
    p <- ggplot2::ggplot(CN, aes(x = Year, y = CN)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::labs(x = "Year", y = "CN", title = "CN Over Years") +
      ggplot2::theme_minimal()
  }


  else
  {
    # Filter rows that contain "B"
    ss_CN <- model_result[["est_std"]][grep("^CN", rownames(model_result[["est_std"]])),]

    # Calculate confidence intervals
    confidence_intervals_CN <- data.frame(
      estimate = ss_CN[, "Estimate"],
      lower = ss_CN[, "Estimate"] - 1.96 * ss_CN[, "Std. Error"],
      upper = ss_CN[, "Estimate"] + 1.96 * ss_CN[, "Std. Error"]
    )

    confidence_intervals_CN$Year <-  model_result[["year"]]

    # Plot recruitment over the years with confidence intervals using ggplot2
    p <- ggplot2::ggplot(confidence_intervals_CN, aes(x = Year, y = estimate)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
      ggplot2::labs(y = "CN", x = "Year", title = "CN Over Years with Confidence Intervals") +
      ggplot2::theme_minimal()
  }
  }
  if(type=="CNA"){

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

  }
  return(p)
}

