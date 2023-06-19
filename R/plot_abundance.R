#' Plot number over the years
#'
#' This function takes a list and a data frame as input, and plots number over the years using ggplot2.
#' @param model_result A list that contains model output. The list should have a "report" component which contains a "N" component representing number.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. The color of the line in the plot. Default is "black".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se_color Character. The color of the confidence interval ribbon. Default is "blue".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @param facet_ncol Number of columns in facet wrap. Default is 3.
#' @param facet_scales Scales for facet wrap. Default is "free".
#' @param type Character. It specifies which the abundance is ploted for "N" or "NA". Default is "N".
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' plot_abundance(model_result, line_size = 1.2, line_color = "red", line_type = "solid")
#' }
plot_abundance <- function(model_result, line_size = 1.2, line_color = "red", line_type = "solid", se = FALSE, se_color = "red", se_alpha = 0.2,type=c("N","NA"), facet_ncol = 3, facet_scales = "free" ){
 if(type=="N"){


   # Extract the number data
  number <- model_result[["report"]][["N"]]

  # Make sure it's a data frame
  if(!is.data.frame(number)){
    number <- as.data.frame(number)
  }

  # Add Year to the number data
  number$Year <-  model_result[["year"]]

  if(!se)
  {
    # Plot number over the years using ggplot2
    p <- ggplot2::ggplot(number, aes(x = Year, y = number)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::labs(x = "Year", y = "number", title = "number Over Years") +
      ggplot2::theme_minimal()
  }


  else
  {
    # Filter rows that contain "N"
    ss_n <- model_result[["est_std"]][grepl("^N(?!A|L)", rownames(model_result[["est_std"]]), perl = TRUE),]

    # Calculate confidence intervals
    confidence_intervals_n <- data.frame(
      estimate = ss_n[, "Estimate"],
      lower = ss_n[, "Estimate"] - 1.96 * ss_n[, "Std. Error"],
      upper = ss_n[, "Estimate"] + 1.96 * ss_n[, "Std. Error"]
    )

    confidence_intervals_n$Year <-  model_result[["year"]]

    # Plot recruitment over the years with confidence intervals using ggplot2
    p <- ggplot2::ggplot(confidence_intervals_n, aes(x = Year, y = estimate)) +
      ggplot2::geom_line(size = line_size, color = line_color, linetype = line_type) +
      ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
      ggplot2::labs(y = "Number", x = "Year", title = "Number Over Years with Confidence Intervals") +
      ggplot2::theme_minimal()
  }
 }
  if (type=="NA"){

    # Extract the na data
    na<- model_result[["report"]][["NA"]]

    # Create Year variable from column names of na (assuming columns are years)
    Year <- model_result[["year"]]

    # Create AgeGroup variable from row names of na
    AgeGroup <- paste0("Age group ", seq_len(nrow(na)))
    AgeGroup <- factor(paste("Age bin ", seq_len(nrow(na))), levels=paste("Age bin ",seq_len(nrow(na))))

    # Convert matrix to data frame in long format
    na_long <- reshape2::melt(na)
    colnames(na_long) <- c("AgeGroup", "Year", "number.age")
    na_long$Year <- Year[as.numeric(na_long$Year)]
    na_long$AgeGroup <- AgeGroup[as.numeric(na_long$AgeGroup)]
    if(!se)
    {
      # Plot fishing mortality over the years using ggplot2
      p <- ggplot2::ggplot(na_long, aes(x = Year, y = number.age)) +
        ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
        ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = "Year", y = "Relative abundance", title = "Number of different age groups per year") +
        ggplot2::theme_minimal()

    }

    else
    {

      # Filter rows that contain "NA"
      ss_na <- model_result[["est_std"]][grep("^NA", rownames(model_result[["est_std"]])),]
      ss_na<-as.data.frame(ss_na)
      # Calculate confidence intervals
      confidence_intervals_na <- data.frame(
        estimate = ss_na[, "Estimate"],
        lower = ss_na[, "Estimate"] - 1.96 * ss_na[, "Std. Error"],
        upper = ss_na[, "Estimate"] + 1.96 * ss_na[, "Std. Error"]
      )


      # Create AgeGroup and Year columns for ss_f
      ss_na$AgeGroup <- rep(paste0("Age group ", seq_len(nrow(na))), times = ncol(na))
      ss_na$Year <- rep(Year, each = nrow(na))



      confidence_intervals_na <- data.frame(
        AgeGroup = ss_na$AgeGroup,
        Year = ss_na$Year,
        estimate = ss_na[, "Estimate"],
        lower = ss_na[, "Estimate"] - 1.96 * ss_na[, "Std. Error"],
        upper = ss_na[, "Estimate"] + 1.96 * ss_na[, "Std. Error"]
      )




      p <- ggplot2::ggplot(confidence_intervals_na, aes(x = Year, y = estimate)) +
        ggplot2::geom_line( size = line_size, color = line_color, linetype = line_type) +
        ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper),  fill = se_color,alpha = se_alpha) +
        ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = "Year", y = "Relative abundance", title = "Number of different age groups per year with Confidence Intervals") +
        ggplot2::theme_minimal()

    }
    }
  return(p)

}

