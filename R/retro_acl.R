#' Retrospective analysis function with plot option
#'
#' @param nyear Number of years to look back in the retrospective analysis.
#' @param data.CatL Data for CatL.
#' @param data.wgt Data for weight.
#' @param data.mat Data for mat.
#' @param rec.age Recreational age.
#' @param nage Number of age.
#' @param M Natural mortality rate.
#' @param sel_L50 Length at 50% selection.
#' @param sel_L95 Length at 95% selection.
#' @param parameters Optional parameters for the ACL model.
#' @param parameters.L Optional L parameters for the ACL model.
#' @param parameters.U Optional U parameters for the ACL model.
#' @param map Optional map for the ACL model.
#' @param len_mid Optional length mid for the ACL model.
#' @param len_border Optional length border for the ACL model.
#' @param plot Logical indicating whether to plot the results. Defaults to FALSE.
#' @param point_size Character. The size of the point in the plot. Default is "solid".
#' @param point_shape Character. The shape of the point in the plot. Default is "solid".
#' @param line_size Line size for the plot. Defaults to 1.
#' @param facet_col Defaults to "NULL".
#' @param facet_row Defaults to "NULL".
#' @param facet_scales Argument passed to `facet_wrap()`. Defaults to "free".
#'
#' @return If `plot` is TRUE, returns a ggplot object. Otherwise, returns a dataframe with the results.
#' @export
#'
#' @examples
#' \dontrun{
#' retro_acl(nyear, data.CatL, data.wgt, data.mat, rec.age, nage, M, sel_L50, sel_L95, plot = TRUE)
#' }

retro_acl <- function(nyear, data.CatL, data.wgt, data.mat, rec.age, nage, M, sel_L50, sel_L95,
                                   parameters = NULL, parameters.L = NULL, parameters.U = NULL,
                                   map = NULL, len_mid = NULL, len_border = NULL, plot = FALSE,
                                   line_size = 1.2, point_size=3,point_shape=21,facet_scales = "free", facet_col = NULL, facet_row = NULL) {

  library(ggplot2)
  library(dplyr)

  results <- data.frame(Year = integer(), Variable = character(), Value = numeric(), RetrospectiveYear = integer(), Rho = numeric())

  # 先获取完整数据模型的结果
  model_result <- run_acl(data.CatL = data.CatL,
                          data.wgt = data.wgt,
                          data.mat = data.mat,
                          rec.age, nage, M, sel_L50, sel_L95,
                          parameters, parameters.L, parameters.U, map, len_mid, len_border)

  # 提取完整的年份数据
  year <- model_result[["year"]]

  # 提取完整的B，SSB，Rec和N数据
  variables <- list(
    B = model_result[["report"]][["B"]],
    Rec = model_result[["report"]][["Rec"]],
    SSB = model_result[["report"]][["SSB"]],
    N = model_result[["report"]][["N"]]
  )

  # 将完整的结果保存到data.frame中
  for (variable_name in names(variables)) {
    temp_full <- data.frame(Year = year,
                            Variable = rep(variable_name, each = length(year)),
                            Value = variables[[variable_name]],
                            RetrospectiveYear = rep(tail(year, 1), length(year)),
                            Rho = rep(NA, length(year)))  # Rho值对于完整数据集为NA
    results <- rbind(results, temp_full)
  }

  for (i in 1:(nyear)) {

    # 计算需要的年份数据
    year1 = ncol(data.CatL) - i

    # 运行ACL模型
    model_result <- run_acl(data.CatL = data.CatL[ , 1:year1],
                            data.wgt = data.wgt[ , 1:year1],
                            data.mat = data.mat[ , 1:year1],
                            rec.age, nage, M, sel_L50, sel_L95,
                            parameters, parameters.L, parameters.U, map, len_mid, len_border)
    year2 <- model_result[["year"]]

    # 提取最后一年的参数值
    variables <- list(
      B = model_result[["report"]][["B"]],
      Rec = model_result[["report"]][["Rec"]],
      SSB = model_result[["report"]][["SSB"]],
      N = model_result[["report"]][["N"]]
    )

    # 提取回溯性分析的年份
    retrospectiveYear <- tail(model_result[["year"]], 1)

    # 对于每个变量，计算rho值并将结果保存到data.frame中
    for (variable_name in names(variables)) {
      # 提取与回溯数据相同年份的原始数据
      original_values <- results[results$Year %in% year2 & results$Variable == variable_name, "Value"]

      # 计算rho值
      if(length(variables[[variable_name]]) != length(original_values)){
        stop("Vectors are not of the same length.")
      } else {
        rho <- cor(variables[[variable_name]], original_values)
      }

      # 将结果保存到data.frame中
      temp <- data.frame(Year = year2,
                         Variable = rep(variable_name, each = length(year2)),
                         Value = variables[[variable_name]],
                         RetrospectiveYear = rep(retrospectiveYear, length(year2)),
                         Rho = rep(rho, length(year2)))
      results <- rbind(results, temp)
    }
  }


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
