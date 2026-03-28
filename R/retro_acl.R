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
#' @param rho_digits Integer. Number of decimal places for Mohn's rho in default plot (when plot=TRUE). Default is 4.
#'   For changing rho_digits without re-computing, use \code{plot_retro(retro, rho_digits = 6)}.
#' @param rho_position Character. Position of rho text: "top_right" (default), "top_left", "bottom_right", "bottom_left".
#' @param rho_size Numeric. Font size of rho text. Default is 3.5.
#' @param ncores Integer. Number of CPU cores for parallel peel computation. 1 = sequential (default).
#'   The full model always runs sequentially first; only the retrospective peels are parallelized.
#'   On Mac/Linux uses forked processes (mclapply); on Windows uses socket cluster (parLapply).
#' @return A list containing \code{results} (data.frame), \code{rho_text}, \code{last_points},
#'   and optionally \code{plot} (if plot=TRUE). Use \code{plot_retro()} to re-plot with different settings.
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
                      line_size = 1.2, point_size=3,point_shape=21,facet_scales = "free", facet_col = NULL, facet_row = NULL,train_times=1, title = NULL, xlab = NULL, ylab = NULL, font_family = NULL, title_size = NULL, axis_title_size = NULL, axis_text_size = NULL, strip_text_size = NULL, legend_text_size = NULL, x_breaks = NULL, base_theme = NULL, title_hjust = NULL, rho_digits = 4, rho_position = "top_right", rho_size = 3.5, ncores = 1) {

  results <- data.frame(Year = integer(), Variable = character(), Value = numeric(), RetrospectiveYear = integer(), Rho = numeric())

  results1 <- data.frame(Year = integer(), Variable = character(), Value = numeric(), RetrospectiveYear = integer(), Rho = numeric())

  t_grand <- proc.time()

  # Get the results of the full data model first
  cat("Fitting full model...\n")
  t_full <- proc.time()
  model_result <- run_acl(data.CatL = data.CatL,
                          data.wgt = data.wgt,
                          data.mat = data.mat,
                          rec.age = rec.age, nage = nage, M = M,
                          sel_L50 = sel_L50, sel_L95 = sel_L95,
                          parameters = parameters, parameters.L = parameters.L,
                          parameters.U = parameters.U, map = map,
                          len_mid = len_mid, len_border = len_border,
                          train_times = train_times)
  t_full_elapsed <- (proc.time() - t_full)[["elapsed"]]
  cat(sprintf("  Full model done: %.1f sec\n", t_full_elapsed))

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

  # --- Worker function: runs one retrospective peel ---
  .fit_one_peel <- function(i) {
    t_peel <- proc.time()
    year1 <- ncol(data.CatL) - i

    peel_result <- run_acl(data.CatL = data.CatL[, 1:year1],
                           data.wgt = data.wgt[, 1:year1],
                           data.mat = data.mat[, 1:year1],
                           rec.age = rec.age, nage = nage, M = M,
                           sel_L50 = sel_L50, sel_L95 = sel_L95,
                           parameters = parameters, parameters.L = parameters.L,
                           parameters.U = parameters.U, map = map,
                           len_mid = len_mid, len_border = len_border,
                           train_times = train_times)

    t_peel_elapsed <- (proc.time() - t_peel)[["elapsed"]]
    cat(sprintf("  Peel %d/%d done: %.1f sec\n", i, nyear, t_peel_elapsed))

    year2 <- peel_result[["year"]]
    variables <- list(
      B   = peel_result[["report"]][["B"]],
      Rec = peel_result[["report"]][["Rec"]],
      SSB = peel_result[["report"]][["SSB"]],
      N   = peel_result[["report"]][["N"]]
    )
    retrospectiveYear <- tail(peel_result[["year"]], 1)

    # Return raw results for post-processing in main process
    list(year2 = year2, variables = variables, retrospectiveYear = retrospectiveYear, peel = i)
  }

  # --- Dispatch: sequential vs parallel ---
  ncores_use <- min(ncores, nyear)
  t_peels <- proc.time()

  if (ncores_use <= 1) {
    cat(sprintf("Running %d retrospective peels sequentially...\n", nyear))
    peel_results <- lapply(1:nyear, .fit_one_peel)
  } else {
    cat(sprintf("Running %d retrospective peels on %d cores", nyear, ncores_use))

    if (.Platform$OS.type == "unix") {
      cat(" [fork: mclapply]...\n")
      peel_results <- parallel::mclapply(1:nyear, .fit_one_peel, mc.cores = ncores_use)
    } else {
      cat(" [socket: parLapply]...\n")
      cl <- parallel::makeCluster(ncores_use)

      parallel::clusterExport(cl, varlist = c(
        "data.CatL", "data.wgt", "data.mat",
        "rec.age", "nage", "M", "sel_L50", "sel_L95",
        "parameters", "parameters.L", "parameters.U",
        "map", "len_mid", "len_border", "train_times",
        ".fit_one_peel"
      ), envir = environment())

      parallel::clusterEvalQ(cl, {
        library(TMB)
        library(ACL)
      })

      peel_results <- parallel::parLapply(cl, 1:nyear, .fit_one_peel)
      parallel::stopCluster(cl)
    }
  }

  # --- Post-process peel results (in main process) ---
  for (pr in peel_results) {
    year2 <- pr$year2
    variables <- pr$variables
    retrospectiveYear <- pr$retrospectiveYear

    for (variable_name in names(variables)) {
      original_values <- results1[results1$Year %in% year2 & results1$Variable == variable_name, "Value"]

      if (length(variables[[variable_name]]) != length(original_values)) {
        print(head(results1[results1$Year %in% year2 & results1$Variable == variable_name, ]))
        print(paste("Processing peel:", pr$peel))
        print(paste("Length of variables[[variable_name]]:", length(variables[[variable_name]])))
        print(paste("Length of original_values:", length(original_values)))
        stop("Vectors are not of the same length.")
      } else {
        rho <- mean((variables[[variable_name]] - original_values) / original_values)
      }

      temp <- data.frame(Year = year2,
                         Variable = rep(variable_name, each = length(year2)),
                         Value = variables[[variable_name]],
                         RetrospectiveYear = rep(retrospectiveYear, length(year2)),
                         Rho = rep(rho, length(year2)))
      results <- rbind(results, temp)
    }
  }

  t_peels_elapsed <- (proc.time() - t_peels)[["elapsed"]]
  t_grand_elapsed <- (proc.time() - t_grand)[["elapsed"]]

  cat(sprintf("\n=== retro_acl complete ===\n"))
  cat(sprintf("  Full model:  %.1f sec\n", t_full_elapsed))
  cat(sprintf("  %d peels:     %.1f sec\n", nyear, t_peels_elapsed))
  cat(sprintf("  Total:       %.1f sec (%.1f min)\n", t_grand_elapsed, t_grand_elapsed / 60))

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
    p <- plot_retro(list(results = results, rho_text = rho_text, last_points = last_points),
                    rho_digits = rho_digits, rho_position = rho_position, rho_size = rho_size,
                    line_size = line_size, point_size = point_size,
                    point_shape = point_shape, facet_scales = facet_scales,
                    facet_col = facet_col, facet_row = facet_row,
                    title = title, xlab = xlab, ylab = ylab, font_family = font_family,
                    title_size = title_size, axis_title_size = axis_title_size,
                    axis_text_size = axis_text_size, strip_text_size = strip_text_size,
                    legend_text_size = legend_text_size, x_breaks = x_breaks,
                    base_theme = base_theme, title_hjust = title_hjust)

    return(list(results = results, rho_text = rho_text, last_points = last_points, plot = p))
  }

  return(list(results = results, rho_text = rho_text, last_points = last_points))
}


#' Plot Retrospective Analysis Results
#'
#' Generate or re-generate the retrospective plot from stored results.
#' This allows changing display options (e.g. rho decimal places, colors,
#' font) without re-running the expensive \code{retro_acl()} computation.
#'
#' @param retro_result A list returned by \code{retro_acl()}, containing
#'   \code{results}, \code{rho_text}, and \code{last_points}.
#' @param rho_digits Integer. Number of decimal places for Mohn's rho display. Default is 4.
#' @param line_size Numeric. Line thickness. Default is 1.2.
#' @param point_size Numeric. Point size for last points. Default is 3.
#' @param point_shape Numeric. Point shape for last points. Default is 21.
#' @param facet_scales Character. Facet scales. Default is "free".
#' @param facet_col Integer or NULL. Number of facet columns.
#' @param facet_row Integer or NULL. Number of facet rows.
#' @param title Character or NULL. Custom plot title.
#' @param xlab Character or NULL. Custom x-axis label.
#' @param ylab Character or NULL. Custom y-axis label.
#' @param font_family Character or NULL. Font family.
#' @param title_size Numeric or NULL. Title size in pt.
#' @param axis_title_size Numeric or NULL. Axis title size in pt.
#' @param axis_text_size Numeric or NULL. Axis tick label size in pt.
#' @param strip_text_size Numeric or NULL. Facet label size in pt.
#' @param legend_text_size Numeric or NULL. Legend text size in pt.
#' @param x_breaks Numeric vector or NULL. Custom x-axis breaks.
#' @param base_theme Character or NULL. Base ggplot2 theme name (e.g. "theme_bw"). NULL = global setting.
#' @param title_hjust Numeric or NULL. Title horizontal alignment: 0 = left, 0.5 = center, 1 = right. NULL = global setting.
#' @param rho_position Character. Position of Mohn's rho text in each facet.
#'   One of \code{"top_right"} (default), \code{"top_left"},
#'   \code{"bottom_right"}, \code{"bottom_left"}.
#' @param rho_size Numeric. Font size of rho text. Default is 3.5.
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' retro <- retro_acl(nyear = 3, ...)
#'
#' # Default plot
#' plot_retro(retro)
#'
#' # Change rho digits
#' plot_retro(retro, rho_digits = 6)
#'
#' # Change rho position
#' plot_retro(retro, rho_position = "top_right")
#' plot_retro(retro, rho_position = "top_left")
#' plot_retro(retro, rho_position = "bottom_right")
#'
#' # Change style
#' plot_retro(retro, rho_digits = 2, font_family = "Times New Roman",
#'            title = "Custom Title", x_breaks = seq(1, 20, by = 5))
#' }
plot_retro <- function(retro_result,
                       rho_digits = 4,
                       rho_position = "top_right",
                       rho_size = 3.5,
                       line_size = 1.2, point_size = 3, point_shape = 21,
                       facet_scales = "free", facet_col = NULL, facet_row = NULL,
                       title = NULL, xlab = NULL, ylab = NULL,
                       font_family = NULL, title_size = NULL,
                       axis_title_size = NULL, axis_text_size = NULL,
                       strip_text_size = NULL, legend_text_size = NULL,
                       x_breaks = NULL, base_theme = NULL, title_hjust = NULL) {

  results     <- retro_result$results
  rho_text    <- retro_result$rho_text
  last_points <- retro_result$last_points

  # Recalculate rho text x position based on rho_position
  x_min <- min(results$Year, na.rm = TRUE)
  x_max <- max(results$Year, na.rm = TRUE)
  x_range <- x_max - x_min

  pos <- match.arg(rho_position, c("top_right", "top_left", "bottom_right", "bottom_left"))
  if (pos %in% c("top_right", "bottom_right")) {
    rho_text$Year <- x_max - x_range * 0.02
    rho_hjust <- 1
  } else {
    rho_text$Year <- x_min + x_range * 0.02
    rho_hjust <- 0
  }
  if (pos %in% c("top_right", "top_left")) {
    rho_y   <- Inf
    rho_vjust <- 1.5
  } else {
    rho_y   <- -Inf
    rho_vjust <- -0.5
  }

  p <- ggplot2::ggplot(results, ggplot2::aes(x = Year, y = Value,
                                             color = RetrospectiveYear,
                                             group = RetrospectiveYear)) +
    ggplot2::geom_line(linewidth = line_size) +
    ggplot2::geom_point(data = last_points, size = point_size, shape = point_shape) +
    ggplot2::facet_wrap(~Variable, scales = facet_scales, ncol = facet_col, nrow = facet_row) +
    .acl_scale_x(x_breaks, n_breaks = 10) +
    .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"),
                  y = "Value", color = "Retrospective Year",
                  title = if (!is.null(title)) title else .acl_title("retro"))

  # Add rho text
  p <- p + ggplot2::geom_text(
    data = rho_text,
    ggplot2::aes(x = Year, label = paste0("rho = ", sprintf(paste0("%.", rho_digits, "f"), Rho))),
    y = rho_y, hjust = rho_hjust, vjust = rho_vjust,
    size = rho_size,
    inherit.aes = FALSE
  )

  return(p)
}
