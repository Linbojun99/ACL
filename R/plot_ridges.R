#' @title Plot ridges of Observed and Estimated Catch-at-Length Over Years
#'
#' @description This function takes the model result as an input and creates two ridgeline plots to
#' visualize the density distribution of observed and estimated log catch at length over different years.
#' The density distribution is calculated by transforming the abundance data with the exponential function.
#'
#' @param model_result A list containing the model result, which includes a data frame of Elog_index and logN_at_len, len_mid, and year.
#' @param ridges_alpha Numeric. Specifies the transparency of the ridgeline plots. Default is 0.8.
#' @param palette Character or character vector. Color palette for the fill.
#'   Built-in palette names: \code{"viridis"} (default), \code{"plasma"},
#'   \code{"inferno"}, \code{"cividis"}, \code{"turbo"}, \code{"magma"},
#'   \code{"ocean"}, \code{"sunset"}, \code{"forest"}, \code{"fire"},
#'   \code{"spectral"}, \code{"RdYlBu"}, \code{"RdYlGn"}, \code{"PiYG"},
#'   \code{"BrBG"}, \code{"rainbow"}, \code{"terrain"}, \code{"heat"},
#'   \code{"topo"}.
#'   Or supply a character vector of colors (e.g. \code{c("#0D1B2A","#2E86AB","#72EFDD")})
#'   which will be interpolated to match the number of years.
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
#' @return A grid arranged ggplot object.
#'
#' @examples
#' \dontrun{
#' # Assume model_result is the result obtained from function 'run_acl'.
#' p <- plot_ridges(model_result)
#' print(p)
#'
#' # Built-in palettes
#' plot_ridges(model_result, palette = "viridis")   # purple-yellow
#' plot_ridges(model_result, palette = "ocean")      # deep blue-teal
#' plot_ridges(model_result, palette = "sunset")     # dark purple-red-gold
#' plot_ridges(model_result, palette = "forest")     # dark green-light green
#' plot_ridges(model_result, palette = "fire")       # black-red-gold
#' plot_ridges(model_result, palette = "spectral")   # red-yellow-blue
#' plot_ridges(model_result, palette = "turbo")      # rainbow-like
#' plot_ridges(model_result, palette = "plasma")     # purple-orange-yellow
#'
#' # Custom colors (auto-interpolated)
#' plot_ridges(model_result, palette = c("#0D1B2A", "#2E86AB", "#72EFDD"))
#' plot_ridges(model_result, palette = c("navy", "white", "firebrick"))
#' }
#' @export
plot_ridges <- function(model_result, ridges_alpha = 0.8,
                        palette = "viridis",
                        title = NULL, xlab = NULL, ylab = NULL,
                        font_family = NULL, title_size = NULL,
                        axis_title_size = NULL, axis_text_size = NULL,
                        strip_text_size = NULL, legend_text_size = NULL,
                        x_breaks = NULL, base_theme = NULL, title_hjust = NULL){

  # Define the data
  Elog_index <- model_result[["report"]][["Elog_index"]]
  logN_at_len <- model_result[["obj"]][["env"]][[".data"]][["logN_at_len"]]
  len_mid <- model_result[["len_mid"]]
  year <- model_result[["year"]]
  n_years <- length(year)

  # --- Build fill scale from palette ---
  fill_scale <- .acl_ridges_palette(palette, n_years)

  # Transform the matrices into data frames and add column names
  df_observed <- as.data.frame(exp(logN_at_len))
  Total1 <- as.vector(colSums(df_observed))
  df_observed <- as.data.frame(sweep(x = df_observed, MARGIN = 2, STATS = Total1, FUN = "/"))
  colnames(df_observed) <- year
  df_observed$len_mid <- len_mid

  df_estimated <- as.data.frame(exp(Elog_index))
  Total2 <- as.vector(colSums(df_estimated))
  df_estimated <- as.data.frame(sweep(x = df_estimated, MARGIN = 2, STATS = Total2, FUN = "/"))
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
    fill_scale +
    ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "length"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), title = if (!is.null(title)) title else .acl_title("CatL_obs_length")) +
    .acl_scale_x(x_breaks, n_breaks = 8) +
    .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

  p_estimated <- ggplot2::ggplot(df_estimated_long, aes(x = len_mid, y = as.factor(year), height = Abundance, fill = as.factor(year))) +
    ggridges::geom_density_ridges(stat = "identity", alpha = ridges_alpha) +
    fill_scale +
    ggplot2::labs(x = if (!is.null(xlab)) xlab else .acl_lab("x", "length"), y = if (!is.null(ylab)) ylab else .acl_lab("y", "abundance"), title = if (!is.null(title)) title else .acl_title("CatL_est_length")) +
    .acl_scale_x(x_breaks, n_breaks = 8) +
    .acl_base_theme(font_family, title_size, axis_title_size, axis_text_size, strip_text_size, legend_text_size, base_theme = base_theme, title_hjust = title_hjust)

  p <- cowplot::plot_grid(p_observed, p_estimated, ncol = 2)

  return(p)
}


#' Build fill scale for ridgeline plots
#' @param palette Character palette name or character vector of colors.
#' @param n Integer. Number of colors needed.
#' @return A ggplot2 scale_fill layer.
#' @keywords internal
.acl_ridges_palette <- function(palette, n) {

  # viridis-family palettes
  viridis_opts <- c("viridis" = "D", "magma" = "A", "inferno" = "B",
                    "plasma" = "C", "cividis" = "E", "turbo" = "H")

  # RColorBrewer diverging palettes
  brewer_div <- c("spectral" = "Spectral", "brbg" = "BrBG",
                  "rdylbu" = "RdYlBu", "rdylgn" = "RdYlGn",
                  "piyg" = "PiYG", "prgn" = "PRGn", "rdbu" = "RdBu")

  if (length(palette) == 1 && is.character(palette)) {
    pal <- tolower(palette)

    # viridis family
    if (pal %in% names(viridis_opts)) {
      return(ggplot2::scale_fill_viridis_d(option = viridis_opts[[pal]], guide = "none"))
    }

    # brewer diverging
    if (pal %in% names(brewer_div)) {
      cols <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(min(n, 11), brewer_div[[pal]])
      )(n)
      return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
    }

    # Custom hand-crafted palettes
    if (pal == "ocean") {
      cols <- grDevices::colorRampPalette(c("#0D1B2A", "#1B3A5C", "#2E86AB", "#48BFE3", "#72EFDD", "#B8F3E8"))(n)
      return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
    }
    if (pal == "sunset") {
      cols <- grDevices::colorRampPalette(c("#1A1A2E", "#16213E", "#533483", "#E94560", "#F38181", "#FCE38A"))(n)
      return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
    }
    if (pal == "forest") {
      cols <- grDevices::colorRampPalette(c("#1B4332", "#2D6A4F", "#40916C", "#52B788", "#95D5B2", "#D8F3DC"))(n)
      return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
    }
    if (pal == "fire") {
      cols <- grDevices::colorRampPalette(c("#03071E", "#370617", "#6A040F", "#D00000", "#E85D04", "#FFBA08"))(n)
      return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
    }
    if (pal == "rainbow") {
      cols <- grDevices::rainbow(n)
      return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
    }
    if (pal == "terrain") {
      cols <- grDevices::terrain.colors(n)
      return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
    }
    if (pal == "heat") {
      cols <- grDevices::heat.colors(n)
      return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
    }
    if (pal == "topo") {
      cols <- grDevices::topo.colors(n)
      return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
    }

    # Fallback: unrecognized name
    message("Unknown palette '", palette, "', using 'viridis'. Available: ",
            paste(c(names(viridis_opts), names(brewer_div),
                    "ocean", "sunset", "forest", "fire",
                    "rainbow", "terrain", "heat", "topo"), collapse = ", "))
    return(ggplot2::scale_fill_viridis_d(option = "D", guide = "none"))
  }

  # Custom color vector — interpolate to n colors
  if (length(palette) >= 2) {
    cols <- grDevices::colorRampPalette(palette)(n)
    return(ggplot2::scale_fill_manual(values = cols, guide = "none"))
  }

  # Default
  ggplot2::scale_fill_viridis_d(option = "D", guide = "none")
}
