#' Plot Abundance over the Years in ACL model
#'
#' This function is specifically designed for visualizing fish abundance data obtained from ACL fishery resource assessments.
#' It can handle and plot three different types of abundance: total number ("N"), number at age ("NA"), and number at length ("NL").
#'
#' @param model_result A list that contains model output. The model output should be the result of a ACL fishery resource assessment,
#' and it should have a "report" component which contains "N", "NA", or "NL" components representing the abundance.
#' @param line_size Numeric. The thickness of the line in the plot. Default is 1.2.
#' @param line_color Character. The color of the line in the plot. Default is "red".
#' @param line_type Character. The type of the line in the plot. Default is "solid".
#' @param se Logical. Whether to calculate and plot standard error as confidence intervals. Default is FALSE.
#' @param se_color Character. The color of the confidence interval ribbon. Default is "red".
#' @param se_alpha Numeric. The transparency of the confidence interval ribbon. Default is 0.2.
#' @param se_type Character. Type of CI display: "ribbon" (shaded area) or "errorbar" (error bars). Default is "ribbon".
#' @param type Character. It specifies which the abundance is ploted for "N" , "NA" or "NL". Default is "N".
#' @param facet_ncol Number of columns in facet wrap. Default is NULL.
#' @param facet_scales Scales for facet wrap. Default is "free".
#' @param return_data A logical indicating whether to return the processed data alongside the plot. Default is FALSE.
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
#' @return A ggplot object representing the plot.
#' @export
#' @examples
#' \dontrun{
#' # Simulate a model_result list to run the function
#' model_result <- run_acl(model)
#'
#' # Type "N" for total number
#' # In this case, we're plotting the total number over the years, without standard error.
#' # line_size, line_color and line_type are customized.
#' plot_abundance(model_result, type = "N", line_size = 1.2, line_color = "red", line_type = "solid")
#'
#' # Type "NA" for number at age
#' # Here, we're plotting the number at age, with standard error (se = TRUE).
#' # The plot will be faceted into 2 columns (facet_ncol = 2).
#' plot_abundance(model_result, type = "NA", se = TRUE, facet_ncol = 2)
#'
#' # Type "NL" for number at length
#' # In this example, we're plotting the number at length, with se = TRUE.
#' # facet_ncol = 4, facet_scales = "fixed".
#' plot_abundance(model_result, type = "NL", se = TRUE,
#'   facet_ncol = 4, facet_scales = "fixed")
#'
#' # data output
#'  plot_result <- plot_abundance(data, type="NA")
#'   plot_with_data <- plot_abundance(data, type="NA", return_data = TRUE)
#'
#' }

plot_abundance <- function(model_result, line_size = 1.5, line_color = "#D32F2F", line_type = "solid", se = FALSE, se_color = "#D32F2F", se_alpha = 0.2, se_type = c("ribbon", "errorbar"),type=c("N","NA","NL"), facet_ncol = NULL, facet_scales = "free", return_data = FALSE, title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL){

  len_label=model_result[["len_label"]]

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
      ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
      ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), title = if (!is.null(title)) title else .acl_title("N")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)
    data_out <-number
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
      ggplot2::geom_line(linewidth = line_size, color = line_color, linetype = line_type) +
      { if (se_type[1] == "ribbon")
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = se_color, alpha = se_alpha)
        else
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), color = se_color, width = 0.3) } +
      ggplot2::labs(y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), title = if (!is.null(title)) title else .acl_title("N_se")) +
      .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

    data_out <-confidence_intervals_n

  }
 }
  if (type=="NA"){

    # Extract the na data
    na<- model_result[["report"]][["NA"]]

    # Create Year variable from column names of na (assuming columns are years)
    Year <- model_result[["year"]]

    # Create AgeGroup variable from row names of na
    AgeGroup <- factor(paste0("Age group ", seq_len(nrow(na))),
                       levels = paste0("Age group ", seq_len(nrow(na))))

    # Convert matrix to data frame in long format
    na_long <- reshape2::melt(na)
    colnames(na_long) <- c("AgeGroup", "Year", "number.age")
    na_long$Year <- Year[as.numeric(na_long$Year)]
    na_long$AgeGroup <- AgeGroup[as.numeric(na_long$AgeGroup)]
    if(!se)
    {
      # Plot fishing mortality over the years using ggplot2
      p <- ggplot2::ggplot(na_long, aes(x = Year, y = number.age)) +
        ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
        ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), title = if (!is.null(title)) title else .acl_title("NA_")) +
        .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

      data_out <-na_long

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
      age_levels <- paste0("Age group ", seq_len(nrow(na)))
      ss_na$AgeGroup <- factor(rep(age_levels, times = ncol(na)), levels = age_levels)
      ss_na$Year <- rep(Year, each = nrow(na))



      confidence_intervals_na <- data.frame(
        AgeGroup = ss_na$AgeGroup,
        Year = ss_na$Year,
        estimate = ss_na[, "Estimate"],
        lower = ss_na[, "Estimate"] - 1.96 * ss_na[, "Std. Error"],
        upper = ss_na[, "Estimate"] + 1.96 * ss_na[, "Std. Error"]
      )




      p <- ggplot2::ggplot(confidence_intervals_na, aes(x = Year, y = estimate)) +
        ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
        { if (se_type[1] == "ribbon")
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = se_color, alpha = se_alpha)
        else
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), color = se_color, width = 0.3) } +
        ggplot2::facet_wrap(~AgeGroup, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), title = if (!is.null(title)) title else .acl_title("NA_se")) +
        .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

      data_out <-confidence_intervals_na

    }
  }
  if(type=="NL"){

    # Extract the NL data
    NL <- model_result[["report"]][["NL"]]

    # Make sure it's a matrix
    if(!is.matrix(NL)){
      NL <- as.matrix(NL)
    }

    # Create Year variable from column names of NL (assuming columns are years)
    Year <- model_result[["year"]]


    # Convert matrix to data frame in long format
    NL_long <- reshape2::melt(NL)
    colnames(NL_long) <- c("LengthGroup", "Year", "Count")
    NL_long$Year <- Year[match(NL_long$Year, 1:length(Year))]
    NL_long$LengthGroup <- factor(.acl_fix_len_labels(len_label),
                                   levels = .acl_fix_len_labels(len_label))

    if(!se)
    {
      # Plot NL over the years using ggplot2
      p <- ggplot2::ggplot(NL_long, aes(x = Year, y = Count)) +
        ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
        ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), title = if (!is.null(title)) title else .acl_title("NL")) +
        .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

      data_out <-NL_long

    }
    else{

      # Filter rows that contain "NL"
      ss_NL <- model_result[["est_std"]][grep("^NL", rownames(model_result[["est_std"]])),]

      ss_NL<-as.data.frame(ss_NL)
      # Calculate confidence intervals
      confidence_intervals_NL <- data.frame(
        estimate = ss_NL[, "Estimate"],
        lower = ss_NL[, "Estimate"] - 1.96 * ss_NL[, "Std. Error"],
        upper = ss_NL[, "Estimate"] + 1.96 * ss_NL[, "Std. Error"]
      )


      # Create LengthGroup and Year columns for ss_NL
      #ss_NL$LengthGroup <- rep(paste0("Lenth bin ", seq_len(nrow(NL))), times = ncol(NL))


      len_levels <- .acl_fix_len_labels(len_label)
      ss_NL$LengthGroup <- factor(rep(len_levels, times = ncol(NL)), levels = len_levels)

      ss_NL$Year <- rep(Year, each = nrow(NL))



      confidence_intervals_NL <- data.frame(
        LengthGroup = ss_NL$LengthGroup,
        Year = ss_NL$Year,
        estimate = ss_NL[, "Estimate"],
        lower = ss_NL[, "Estimate"] - 1.96 * ss_NL[, "Std. Error"],
        upper = ss_NL[, "Estimate"] + 1.96 * ss_NL[, "Std. Error"]
      )



      p <- ggplot2::ggplot(confidence_intervals_NL, aes(x = Year, y = estimate)) +
        ggplot2::geom_line( linewidth = line_size, color = line_color, linetype = line_type) +
        { if (se_type[1] == "ribbon")
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = se_color, alpha = se_alpha)
        else
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), color = se_color, width = 0.3) } +
        ggplot2::facet_wrap(~LengthGroup, ncol = facet_ncol, scales = facet_scales) +
        ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), title = if (!is.null(title)) title else .acl_title("NL_se")) +
        .acl_scale_x(x_breaks, n_breaks = 10) +
      .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

      data_out <-confidence_intervals_NL


    }
  }
  if (return_data) {
    return(list(plot = p, data = data_out))
  } else {
    return(p)
  }
}

