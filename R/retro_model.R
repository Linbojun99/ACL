utils::globalVariables(c("Variable", "RetrospectiveYear"))

#' Retrospective Analysis for ACL or ALSCL Models
#'
#' Conduct a retrospective analysis by sequentially removing the most recent
#' years of data and re-fitting the model. Computes Mohn's rho for SSB,
#' Recruitment, Biomass, and Abundance to quantify retrospective bias.
#' Supports both ACL and ALSCL model types via the `model_type` argument.
#'
#' @param nyear Integer. Number of years to peel (remove) sequentially. Default is 5.
#' @param data.CatL The Catch-at-Length data.
#' @param data.wgt The weight data.
#' @param data.mat The maturity data.
#' @param rec.age The recruitment age.
#' @param nage The number of age classes.
#' @param M The natural mortality rate.
#' @param sel_L50 The length at 50 percent selectivity.
#' @param sel_L95 The length at 95 percent selectivity.
#' @param model_type Character. Model to use: `"acl"` (default) or `"alscl"`.
#' @param growth_step Numeric. Growth transition time step for ALSCL. Default is 1. Ignored for ACL.
#' @param parameters Optional custom starting parameter list.
#' @param parameters.L Optional lower bounds for parameters.
#' @param parameters.U Optional upper bounds for parameters.
#' @param map Optional TMB map argument for fixing parameters.
#' @param len_mid Optional numeric vector of length bin midpoints.
#' @param len_border Optional numeric vector of length bin borders.
#' @param len_lower Optional numeric vector of lower bin boundaries (ALSCL only).
#' @param len_upper Optional numeric vector of upper bin boundaries (ALSCL only).
#' @param train_times Numeric. Number of optimization restarts. Default is 1.
#' @param ncores Integer. Number of CPU cores for parallel peel computation. Default is 1.
#' @param silent Logical. If TRUE, suppress all progress messages. Default is FALSE.
#' @param plot Logical. If TRUE, include plot in return value. Default is FALSE.
#' @param line_size Numeric. Line thickness. Default is 1.2.
#' @param point_size Numeric. Point size for endpoints. Default is 3.
#' @param point_shape Numeric. Point shape for endpoints. Default is 21.
#' @param facet_scales Character. Facet scales argument. Default is "free".
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
#' @param base_theme Character or NULL. Base ggplot2 theme name.
#' @param title_hjust Numeric or NULL. Title horizontal alignment.
#' @param rho_digits Integer. Decimal places for Mohn's rho display. Default is 4.
#' @param rho_position Character. Position of rho text. Default is "top_right".
#' @param rho_size Numeric. Font size of rho text. Default is 3.5.
#' @return A list containing `results`, `rho_text`, `last_points`, `model_type`,
#'   and optionally `plot`.
#' @export
#' @examples
#' \dontrun{
#' retro <- retro_model(nyear = 5, data.CatL, data.wgt, data.mat,
#'                      rec.age = 1, nage = 7, M = 0.8,
#'                      sel_L50 = 28, sel_L95 = 36, model_type = "acl")
#' plot_retro(retro)
#' }
retro_model <- function(nyear = 5, data.CatL, data.wgt, data.mat,
                        rec.age, nage, M, sel_L50, sel_L95,
                        model_type = c("acl", "alscl"),
                        growth_step = 1,
                        parameters = NULL, parameters.L = NULL, parameters.U = NULL,
                        map = NULL, len_mid = NULL, len_border = NULL,
                        len_lower = NULL, len_upper = NULL,
                        train_times = 1, ncores = 1, silent = FALSE,
                        plot = FALSE,
                        line_size = 1.2, point_size = 3, point_shape = 21,
                        facet_scales = "free", facet_col = NULL, facet_row = NULL,
                        title = NULL, xlab = NULL, ylab = NULL,
                        font_family = NULL, title_size = NULL,
                        axis_title_size = NULL, axis_text_size = NULL,
                        strip_text_size = NULL, legend_text_size = NULL,
                        x_breaks = NULL, base_theme = NULL, title_hjust = NULL,
                        rho_digits = 4, rho_position = "top_right", rho_size = 3.5) {

  model_type <- match.arg(model_type)

  if (silent) {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  .fit_model <- function(CatL_sub, wgt_sub, mat_sub) {
    if (model_type == "acl") {
      run_acl(data.CatL = CatL_sub, data.wgt = wgt_sub, data.mat = mat_sub,
              rec.age = rec.age, nage = nage, M = M,
              sel_L50 = sel_L50, sel_L95 = sel_L95,
              parameters = parameters, parameters.L = parameters.L,
              parameters.U = parameters.U, map = map,
              len_mid = len_mid, len_border = len_border,
              train_times = train_times, silent = TRUE)
    } else {
      run_alscl(data.CatL = CatL_sub, data.wgt = wgt_sub, data.mat = mat_sub,
                rec.age = rec.age, nage = nage, M = M,
                sel_L50 = sel_L50, sel_L95 = sel_L95,
                growth_step = growth_step,
                parameters = parameters, parameters.L = parameters.L,
                parameters.U = parameters.U, map = map,
                len_mid = len_mid, len_border = len_border,
                len_lower = len_lower, len_upper = len_upper,
                train_times = train_times, silent = TRUE)
    }
  }

  results  <- data.frame(Year = integer(), Variable = character(),
                         Value = numeric(), RetrospectiveYear = integer(),
                         Rho = numeric())
  results1 <- data.frame(Year = integer(), Variable = character(),
                         Value = numeric(), RetrospectiveYear = integer(),
                         Rho = numeric())

  t_grand <- proc.time()

  cat(sprintf("Fitting full %s model...\n", toupper(model_type)))
  t_full <- proc.time()
  model_result <- .fit_model(data.CatL, data.wgt, data.mat)
  t_full_elapsed <- (proc.time() - t_full)[["elapsed"]]
  cat(sprintf("  Full model done: %.1f sec\n", t_full_elapsed))

  year <- model_result[["year"]]

  rec_val <- model_result[["report"]][["R"]]
  if (is.null(rec_val)) rec_val <- model_result[["report"]][["Rec"]]

  variables <- list(
    B   = model_result[["report"]][["B"]],
    Rec = rec_val,
    SSB = model_result[["report"]][["SSB"]],
    N   = model_result[["report"]][["N"]]
  )

  for (variable_name in names(variables)) {
    v <- variables[[variable_name]]
    if (is.null(v) || length(v) != length(year)) next
    temp_full <- data.frame(
      Year              = year,
      Variable          = rep(variable_name, length(year)),
      Value             = as.numeric(v),
      RetrospectiveYear = rep(tail(year, 1), length(year)),
      Rho               = rep(NA, length(year)))
    results1 <- rbind(results1, temp_full)
  }

  .fit_one_peel <- function(i) {
    t_peel <- proc.time()
    year1 <- ncol(data.CatL) - i

    peel_result <- .fit_model(data.CatL[, 1:year1],
                              data.wgt[, 1:year1],
                              data.mat[, 1:year1])

    t_peel_elapsed <- (proc.time() - t_peel)[["elapsed"]]
    cat(sprintf("  Peel %d/%d done: %.1f sec\n", i, nyear, t_peel_elapsed))

    year2 <- peel_result[["year"]]
    peel_rec <- peel_result[["report"]][["R"]]
    if (is.null(peel_rec)) peel_rec <- peel_result[["report"]][["Rec"]]

    peel_vars <- list(
      B   = peel_result[["report"]][["B"]],
      Rec = peel_rec,
      SSB = peel_result[["report"]][["SSB"]],
      N   = peel_result[["report"]][["N"]]
    )

    retrospectiveYear <- tail(year2, 1)
    list(year2 = year2, variables = peel_vars,
         retrospectiveYear = retrospectiveYear, peel = i)
  }

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
        "growth_step", "model_type",
        "parameters", "parameters.L", "parameters.U",
        "map", "len_mid", "len_border",
        "len_lower", "len_upper", "train_times",
        ".fit_model", ".fit_one_peel"
      ), envir = environment())
      parallel::clusterEvalQ(cl, {
        library(TMB)
        library(ALSCL)
      })
      peel_results <- parallel::parLapply(cl, 1:nyear, .fit_one_peel)
      parallel::stopCluster(cl)
    }
  }

  for (pr in peel_results) {
    year2 <- pr$year2
    peel_vars <- pr$variables
    retrospectiveYear <- pr$retrospectiveYear

    for (variable_name in names(peel_vars)) {
      v <- peel_vars[[variable_name]]
      if (is.null(v)) next
      original_values <- results1[results1$Year %in% year2 &
                                    results1$Variable == variable_name, "Value"]
      if (length(v) != length(original_values)) {
        warning(sprintf("Peel %d: length mismatch for %s, skipping.",
                        pr$peel, variable_name))
        next
      }
      rho <- mean((as.numeric(v) - original_values) / original_values)
      temp <- data.frame(
        Year              = year2,
        Variable          = rep(variable_name, length(year2)),
        Value             = as.numeric(v),
        RetrospectiveYear = rep(retrospectiveYear, length(year2)),
        Rho               = rep(rho, length(year2)))
      results <- rbind(results, temp)
    }
  }

  t_peels_elapsed <- (proc.time() - t_peels)[["elapsed"]]
  t_grand_elapsed <- (proc.time() - t_grand)[["elapsed"]]

  cat(sprintf("\n=== retro_model (%s) complete ===\n", toupper(model_type)))
  cat(sprintf("  Full model:  %.1f sec\n", t_full_elapsed))
  cat(sprintf("  %d peels:     %.1f sec\n", nyear, t_peels_elapsed))
  cat(sprintf("  Total:       %.1f sec (%.1f min)\n", t_grand_elapsed, t_grand_elapsed / 60))

  results <- rbind(results1, results)
  results$RetrospectiveYear <- factor(
    results$RetrospectiveYear,
    levels = sort(unique(results$RetrospectiveYear), decreasing = TRUE))

  xpos <- min(results$Year) + (max(results$Year) - min(results$Year)) / 5

  rho_text <- results %>%
    dplyr::group_by(Variable) %>%
    dplyr::filter(RetrospectiveYear == min(as.numeric(as.character(RetrospectiveYear)), na.rm = TRUE)) %>%
    dplyr::filter(Year == max(Year, na.rm = TRUE)) %>%
    dplyr::summarise(Rho = dplyr::first(Rho), Year = xpos) %>%
    dplyr::ungroup()

  last_points <- results %>%
    dplyr::group_by(Variable, RetrospectiveYear) %>%
    dplyr::filter(Year == max(Year)) %>%
    dplyr::ungroup()

  output <- list(results = results, rho_text = rho_text,
                 last_points = last_points, model_type = model_type)

  if (plot) {
    p <- plot_retro(output,
                    rho_digits = rho_digits, rho_position = rho_position, rho_size = rho_size,
                    line_size = line_size, point_size = point_size,
                    point_shape = point_shape, facet_scales = facet_scales,
                    facet_col = facet_col, facet_row = facet_row,
                    title = title, xlab = xlab, ylab = ylab, font_family = font_family,
                    title_size = title_size, axis_title_size = axis_title_size,
                    axis_text_size = axis_text_size, strip_text_size = strip_text_size,
                    legend_text_size = legend_text_size, x_breaks = x_breaks,
                    base_theme = base_theme, title_hjust = title_hjust)
    output$plot <- p
  }

  return(output)
}


#' Retrospective Analysis for ACL Model
#'
#' Convenience wrapper that calls `retro_model(..., model_type = "acl")`.
#' See [retro_model()] for full documentation of all parameters.
#'
#' @param nyear Integer. Number of years to peel. Default is 5.
#' @param data.CatL The Catch-at-Length data.
#' @param data.wgt The weight data.
#' @param data.mat The maturity data.
#' @param rec.age The recruitment age.
#' @param nage The number of age classes.
#' @param M The natural mortality rate.
#' @param sel_L50 Length at 50 percent selectivity.
#' @param sel_L95 Length at 95 percent selectivity.
#' @param ... Additional arguments passed to [retro_model()].
#' @return A list. See [retro_model()].
#' @seealso [retro_model()], [retro_alscl()], [plot_retro()]
#' @export
retro_acl <- function(nyear = 5, data.CatL, data.wgt, data.mat,
                      rec.age, nage, M, sel_L50, sel_L95, ...) {
  retro_model(nyear = nyear, data.CatL = data.CatL, data.wgt = data.wgt,
              data.mat = data.mat, rec.age = rec.age, nage = nage, M = M,
              sel_L50 = sel_L50, sel_L95 = sel_L95,
              model_type = "acl", ...)
}


#' Retrospective Analysis for ALSCL Model
#'
#' Convenience wrapper that calls `retro_model(..., model_type = "alscl")`.
#' See [retro_model()] for full documentation of all parameters.
#'
#' @param nyear Integer. Number of years to peel. Default is 5.
#' @param data.CatL The Catch-at-Length data.
#' @param data.wgt The weight data.
#' @param data.mat The maturity data.
#' @param rec.age The recruitment age.
#' @param nage The number of age classes.
#' @param M The natural mortality rate.
#' @param sel_L50 Length at 50 percent selectivity.
#' @param sel_L95 Length at 95 percent selectivity.
#' @param growth_step Numeric. Growth transition time step. Default is 1.
#' @param ... Additional arguments passed to [retro_model()].
#' @return A list. See [retro_model()].
#' @seealso [retro_model()], [retro_acl()], [plot_retro()]
#' @export
retro_alscl <- function(nyear = 5, data.CatL, data.wgt, data.mat,
                        rec.age, nage, M, sel_L50, sel_L95,
                        growth_step = 1, ...) {
  retro_model(nyear = nyear, data.CatL = data.CatL, data.wgt = data.wgt,
              data.mat = data.mat, rec.age = rec.age, nage = nage, M = M,
              sel_L50 = sel_L50, sel_L95 = sel_L95,
              model_type = "alscl", growth_step = growth_step, ...)
}


#' Plot Retrospective Analysis Results
#'
#' Generate or re-generate the retrospective plot from stored results.
#' This allows changing display options without re-running the computation.
#'
#' @param retro_result A list returned by `retro_model()`, `retro_acl()`, or `retro_alscl()`.
#' @param rho_digits Integer. Decimal places for Mohn's rho. Default is 4.
#' @param rho_position Character. Position of rho text. Default is "top_right".
#' @param rho_size Numeric. Font size of rho text. Default is 3.5.
#' @param line_size Numeric. Line thickness. Default is 1.2.
#' @param point_size Numeric. Point size. Default is 3.
#' @param point_shape Numeric. Point shape. Default is 21.
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
#' @param base_theme Character or NULL. Base ggplot2 theme name.
#' @param title_hjust Numeric or NULL. Title horizontal alignment.
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' retro <- retro_acl(nyear = 5, data.CatL, data.wgt, data.mat,
#'                    rec.age = 1, nage = 7, M = 0.8,
#'                    sel_L50 = 28, sel_L95 = 36)
#' plot_retro(retro)
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
    rho_y <- Inf
    rho_vjust <- 1.5
  } else {
    rho_y <- -Inf
    rho_vjust <- -0.5
  }

  auto_title <- if (!is.null(retro_result$model_type)) {
    paste0("Retrospective Analysis (", toupper(retro_result$model_type), ")")
  } else {
    "Retrospective Analysis"
  }

  p <- ggplot2::ggplot(results, ggplot2::aes(x = Year, y = Value,
                                              color = RetrospectiveYear,
                                              group = RetrospectiveYear)) +
    ggplot2::geom_line(linewidth = line_size) +
    ggplot2::geom_point(data = last_points, size = point_size, shape = point_shape) +
    ggplot2::facet_wrap(~Variable, scales = facet_scales, ncol = facet_col, nrow = facet_row) +
    .acl_scale_x(x_breaks, n_breaks = 10) +
    .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size,
                    strip_text_size, legend_text_size,
                    base_theme = base_theme, title_hjust = title_hjust) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "year"),
                  y = "Value", color = "Retrospective Year",
                  title = if (!is.null(title)) title else auto_title)

  p <- p + ggplot2::geom_text(
    data = rho_text,
    ggplot2::aes(x = Year,
                 label = paste0("rho = ", sprintf(paste0("%.", rho_digits, "f"), Rho))),
    y = rho_y, hjust = rho_hjust, vjust = rho_vjust,
    size = rho_size,
    inherit.aes = FALSE
  )

  return(p)
}
