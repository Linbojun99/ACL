#' Plot Deviation of Recruitment (R) and Fishing Mortality (F) Over Years
#'
#' This function calculates the deviations of R and F in the ACL model results,
#' which are used to measure the deviance of the model. These deviations are calculated
#' as the difference between the observed and expected values, with the model's residuals
#' as the measurement errors. This function can plot either R or F deviation over years,
#' depending on the 'type' parameter.
#'
#' @param model_result A list that contains the Annual Catch Limit (ACL) model output.
#' @param se Logical, whether to draw the standard error bar.
#' @param line_size Numeric, the line size.
#' @param line_color Character, the line color.
#' @param line_type Character, the line type.
#' @param point_color Character. The color of the point in the plot. Default is "white".
#' @param point_size Character. The size of the point in the plot. Default is "solid".
#' @param point_shape Character. The shape of the point in the plot. Default is "solid".
#' @param se_color Character, the color of the standard error.
#' @param se_width Numeric, the width of the standard error.
#' @param log Logical, whether to apply an exponential transformation to the results.
#' @param facet_ncol Number of columns in facet wrap. Default is NULL.
#' @param facet_scales Scales for facet wrap. Default is "free".
#' @param type Character. It specifies whether the deviation plot is for "R" or "F". Default is "R".
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{
#' # If you want to plot the deviation of recruitment (R) with standard error bars
#' plot_deviance(model_result = your_model_results, se = TRUE, type = "R",
#'               point_size = 1, point_color = "black", point_shape = 1,
#'               line_size = 1, line_color = "black", line_type = "solid",
#'               se_color = "blue", se_alpha = 0.4)
#'
#' # If you do not want to plot the standard error bars
#' plot_deviance(model_result = your_model_results, se = FALSE, type = "R",
#'               point_size = 1, point_color = "black", point_shape = 1,
#'               line_size = 1, line_color = "black", line_type = "solid")
#'
#' # If you want to plot the deviation of fishing mortality (F) with standard error bars
#' plot_deviance(model_result = your_model_results, se = TRUE, type = "F",
#'               point_size = 1, point_color = "black", point_shape = 1,
#'               line_size = 1, line_color = "black", line_type = "solid",
#'               se_color = "blue", se_alpha = 0.4)
#' }

plot_deviance <- function(model_result, se = TRUE, point_size=3,point_color="white",point_shape=21 ,line_size = 1, line_color = "black", line_type = "solid",se_color="black", se_width=0.5,facet_ncol = NULL, facet_scales = "free",log=T,type=c("R","F")) {

  if(type=="R"){
  dev_log_R<-model_result[["est_std"]][grep("^dev_log_R", rownames(model_result[["est_std"]])),]
  # Make sure it's a data frame
  if(!is.data.frame(dev_log_R)){
    dev_log_R <- as.data.frame(dev_log_R)
  }
  confidence_intervals_dev_log_R <- data.frame(
    estimate = dev_log_R[, "Estimate"],
    lower = dev_log_R[, "Estimate"] - 1.96 * dev_log_R[, "Std. Error"],
    upper = dev_log_R[, "Estimate"] + 1.96 * dev_log_R[, "Std. Error"]
  )


  # Add Year to the SSB data
  confidence_intervals_dev_log_R$Year <-  model_result[["year"]]


  # If log = FALSE, apply exp function to estimate, lower, and upper
  if (!log) {
    confidence_intervals_dev_log_R[, c("estimate", "lower", "upper")] <- exp(confidence_intervals_dev_log_R[, c("estimate", "lower", "upper")])
  }



   plot <- ggplot2::ggplot(confidence_intervals_dev_log_R, ggplot2::aes(x = Year, y = estimate)) +
    ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
    ggplot2::geom_point(size=point_size,fill =point_color,shape=point_shape) +
    ggplot2::labs(y = "deviacne_Recruiment", x = "Year", title = "deviacne_Recruiment Over Years") +
    ggplot2::theme_minimal()



  if (se) {


    plot <- plot + ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), color = se_color, width = se_width)+
      ggplot2::geom_point(size=point_size,fill=point_color,shape=point_shape)

  }
  }
  if(type=="F"){

    dev_log_F<-model_result[["est_std"]][grep("^dev_log_F", rownames(model_result[["est_std"]])),]
    dev_log_F<-as.data.frame(dev_log_F)
    # Calculate confidence intervals
    confidence_intervals_dev_log_F <- data.frame(
      estimate = dev_log_F[, "Estimate"],
      lower = dev_log_F[, "Estimate"] - 1.96 * dev_log_F[, "Std. Error"],
      upper = dev_log_F[, "Estimate"] + 1.96 * dev_log_F[, "Std. Error"]
    )


    # Create AgeGroup and Year columns for ss_f
    F<- model_result[["report"]][["F"]]

    dev_log_F$AgeGroup <- rep(paste0("Age group ", seq_len(nrow(F))), times = ncol(F))
    dev_log_F$Year <- rep(model_result[["year"]], each = nrow(F))



    confidence_intervals_dev_log_F <- data.frame(
      AgeGroup = dev_log_F$AgeGroup,
      Year = dev_log_F$Year,
      estimate = dev_log_F[, "Estimate"],
      lower = dev_log_F[, "Estimate"] - 1.96 * dev_log_F[, "Std. Error"],
      upper = dev_log_F[, "Estimate"] + 1.96 * dev_log_F[, "Std. Error"]
    )


    # If log = FALSE, apply exp function to estimate, lower, and upper
    if (!log) {
      confidence_intervals_dev_log_F[, c("estimate", "lower", "upper")] <- exp(confidence_intervals_dev_log_F[, c("estimate", "lower", "upper")])
    }



    plot <- ggplot2::ggplot(confidence_intervals_dev_log_F, ggplot2::aes(x = Year, y = estimate)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_point(size=point_size,fill =point_color,shape=point_shape) +
      ggplot2::labs(y = "deviacne_fishing_mortality", x = "Year", title = "deviacne_fishing_mortality Over Years") +
      ggplot2::theme_minimal()+
      ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales)




    if (se) {


      plot <- plot + ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), color = se_color, width = se_width)+
        ggplot2::geom_point(size=point_size,fill=point_color,shape=point_shape)

    }

  }
  return(plot)
}
















