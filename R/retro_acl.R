#' Retrospective Analysis in Annual Catch Limit (ACL) with Plotting Option
#' @description
#' Conduct a retrospective analysis using an Annual Catch Limit (ACL) model in fisheries stock assessment.
#' The model calculates Mohn's rho, a measure of retrospective bias in the model, which helps in understanding
#' potential biases in the ACL calculations and for setting appropriate catch limits.
#' Mohn's rho is calculated by comparing model parameters from a retrospective analysis with model parameters
#' from the full data set.
#'
#' @param nyear The number of years to look back in the retrospective analysis.
#' @param data.CatL The Catch-at-Length data.
#' @param data.wgt The weight data.
#' @param data.mat The maturity data.
#' @param rec.age The recreational age.
#' @param nage The number of age.
#' @param M The natural mortality rate.
#' @param sel_L50 The length at 50% selection.
#' @param sel_L95 The length at 95% selection.
#' @param parameters Optional parameters for the ACL model.
#' @param parameters.L Optional lower limit parameters for the ACL model.
#' @param parameters.U Optional upper limit parameters for the ACL model.
#' @param map Optional map for the ACL model.
#' @param len_mid Optional length mid for the ACL model.
#' @param len_border Optional length border for the ACL model.
#' @param plot Logical indicating whether to plot the results. Defaults to FALSE.
#' @param line_size The line size for the plot. Defaults to 1.2.
#' @param point_size The size of the point in the plot. Defaults to 3.
#' @param point_shape The shape of the point in the plot. Default is 21.
#' @param facet_scales Argument passed to `facet_wrap()`. Defaults to "free".
#' @param facet_col The number of columns for facet_wrap. Defaults to NULL.
#' @param facet_row The number of rows for facet_wrap. Defaults to NULL.
#' @param train_times Numeric, the number of times the model is to be trained,
#'
#' @return If `plot` is TRUE, returns a ggplot object and a dataframe with the results. Otherwise, returns only the dataframe with the results.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume we have the necessary data and parameters for the ACL model
#' data.CatL <- get_CatL_data()
#' data.wgt <- get_wgt_data()
#' data.mat <- get_mat_data()
#' rec.age <- 2
#' nage <- 10
#' M <- 0.2
#' sel_L50 <- 20
#' sel_L95 <- 30
#'
#' # Conduct a retrospective analysis for the past 5 years
#' retro_acl(nyear = 5, data.CatL, data.wgt, data.mat, rec.age, nage, M, sel_L50, sel_L95, plot = TRUE)
#' }

retro_acl <- function(nyear, data.CatL, data.wgt, data.mat, rec.age, nage, M, sel_L50, sel_L95,
                                   parameters = NULL, parameters.L = NULL, parameters.U = NULL,
                                   map = NULL, len_mid = NULL, len_border = NULL, plot = FALSE,
                                   line_size = 1.2, point_size=3,point_shape=21,facet_scales = "free", facet_col = NULL, facet_row = NULL,train_times=1) {

  library(ggplot2)
  library(dplyr)

  results <- data.frame(Year = integer(), Variable = character(), Value = numeric(), RetrospectiveYear = integer(), Rho = numeric())

  results1 <- data.frame(Year = integer(), Variable = character(), Value = numeric(), RetrospectiveYear = integer(), Rho = numeric())

  # Get the results of the full data model first
  model_result <- run_acl(data.CatL = data.CatL,
                          data.wgt = data.wgt,
                          data.mat = data.mat,
                          rec.age, nage, M, sel_L50, sel_L95,
                          parameters, parameters.L, parameters.U, map, len_mid, len_border,train_times)

  # Extracting complete year data
  year <- model_result[["year"]]

  # Extracts complete B, SSB, Rec and N data
  variables <- list(
    B = model_result[["report"]][["B"]],
    Rec = model_result[["report"]][["Rec"]],
    SSB = model_result[["report"]][["SSB"]],
    N = model_result[["report"]][["N"]]
  )

  # Save the complete result to data.frame
  for (variable_name in names(variables)) {
    temp_full <- data.frame(Year = year,
                            Variable = rep(variable_name, each = length(year)),
                            Value = variables[[variable_name]],
                            RetrospectiveYear = rep(tail(year, 1), length(year)),
                            Rho = rep(NA, length(year)))  # Rho值对于完整数据集为NA
    results1 <- rbind(results1, temp_full)
  }

  for (i in 1:(nyear)) {

    # Calculate the required year data
    year1 = ncol(data.CatL) - i

    # Running the ACL model
    model_result <- run_acl(data.CatL = data.CatL[ , 1:year1],
                            data.wgt = data.wgt[ , 1:year1],
                            data.mat = data.mat[ , 1:year1],
                            rec.age, nage, M, sel_L50, sel_L95,
                            parameters, parameters.L, parameters.U, map, len_mid, len_border)
    year2 <- model_result[["year"]]

    # Extracting the last year's parameter values
    variables <- list(
      B = model_result[["report"]][["B"]],
      Rec = model_result[["report"]][["Rec"]],
      SSB = model_result[["report"]][["SSB"]],
      N = model_result[["report"]][["N"]]
    )

    # Year of retrospective analysis extracted
    retrospectiveYear <- tail(model_result[["year"]], 1)


    # For each variable, calculate the rho value and save the result to data.frame
    for (variable_name in names(variables)) {
      # Extracts the original data for the same year as the backtracked data
      original_values <- results1[results1$Year %in% year2 & results1$Variable == variable_name, "Value"]


      # Calculate rho value
      if(length(variables[[variable_name]]) != length(original_values)){

        # Before extracting original_values
        print(head(results1[results1$Year %in% year2 & results1$Variable == variable_name, ]))


        print(paste("Processing year: ", year2))
        print(paste("Length of variables[[variable_name]]: ", length(variables[[variable_name]])))
        print(paste("Length of original_values: ", length(original_values)))

         stop("Vectors are not of the same length.")
      } else {
        # Use Mohn's rho instead of correlation
        rho <- mean((variables[[variable_name]] - original_values) / original_values)
      }

      # Save the results to data.frame
      temp <- data.frame(Year = year2,
                         Variable = rep(variable_name, each = length(year2)),
                         Value = variables[[variable_name]],
                         RetrospectiveYear = rep(retrospectiveYear, length(year2)),
                         Rho = rep(rho, length(year2)))
      results <- rbind(results, temp)
    }
  }
  results <- rbind(results1, results)


  results$RetrospectiveYear <- factor(results$RetrospectiveYear, levels = sort(unique(results$RetrospectiveYear), decreasing = TRUE))

  # Calculate the position of x axis where we want to place the text
  xpos <- min(results$Year) + (max(results$Year) - min(results$Year)) / 5

  # Only display rho value of the last retrospective year for each Variable
  rho_text <- results %>%
    group_by(Variable) %>%
    filter(RetrospectiveYear == min(as.numeric(as.character(RetrospectiveYear)), na.rm = TRUE)) %>%  # Only keep the smallest RetrospectiveYear

    filter(Year == max(Year,na.rm = TRUE)) %>%
    summarise(Rho = first(Rho), Year = xpos) %>%  # Add Year column
    ungroup()

  # Get the last point of each RetrospectiveYear for each Variable
  last_points <- results %>%
    group_by(Variable, RetrospectiveYear) %>%
    filter(Year == max(Year)) %>%
    ungroup()

  if (plot) {
    # Define plot
    p <- ggplot(results, aes(x = Year, y = Value, color = RetrospectiveYear, group = RetrospectiveYear)) +
      geom_line(size = line_size) +
      geom_point(data = last_points, size = point_size,shape=point_shape) +  # Add geom_point for the last point
      facet_wrap(~Variable, scales = facet_scales, ncol = facet_col, nrow = facet_row) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(x = "Year", y = "Value", color = "Retrospective Year")

    # Add rho values as text at the left top corner of each facet
    p <- p + geom_text(data = rho_text, aes(x = Year, label = paste0("rho = ", round(Rho, 4))),
                       y = Inf, hjust = 0, vjust = 1,
                       inherit.aes = FALSE)

    return(list(results = results, plot = p))
  }

  return(results)
}
