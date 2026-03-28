#' ACL Plot Theme Configuration
#'
#' Global settings for all ACL plot functions. Change titles, axis labels,
#' font family, colors, and line sizes in one place.
#'
#' @section Usage:
#' \preformatted{
#' # View current settings
#' acl_theme()
#'
#' # Change font
#' acl_theme_set(font_family = "Times New Roman")
#'
#' # Change base theme
#' acl_theme_set(base_theme = "theme_bw")
#' acl_theme_set(base_theme = "theme_classic")
#'
#' # Change a title
#' acl_theme_set(titles = list(N = "My Custom Title"))
#'
#' # Reset to defaults
#' acl_theme_reset()
#' }
#'
#' @name acl_theme
NULL

# ---------------------------------------------------------------------------
# Default configuration (internal)
# ---------------------------------------------------------------------------
.acl_defaults <- list(

  # --- Base ggplot2 theme --------------------------------------------------
  base_theme = "theme_bw",   # "theme_bw", "theme_minimal", "theme_classic", "theme_gray", "theme_light", "theme_linedraw", "theme_void"

  # --- Font ----------------------------------------------------------------
  font_family = "Arial",

  # --- Text sizes (in pt) -------------------------------------------------
  title_size      = 14,    # plot title
  title_hjust     = 0.5,   # title alignment: 0 = left, 0.5 = center, 1 = right
  axis_title_size = 12,    # axis title (x/y label)
  axis_text_size  = 10,    # axis tick labels
  strip_text_size = 10,    # facet panel labels
  legend_text_size = 10,   # legend text

  # --- Axis settings -------------------------------------------------------
  x_breaks = NULL,         # NULL = auto (pretty_breaks); or numeric vector e.g. seq(1,20,2)
  x_expand = c(0.01, 0.01),  # expansion multiplier for x-axis

  # --- Default colors and line sizes ---------------------------------------
  line_color  = "#D32F2F",
  line_size   = 1.5,
  se_color    = "#D32F2F",
  se_alpha    = 0.2,

  # --- Titles --------------------------------------------------------------
  titles = list(
    # plot_abundance
    N       = "Estimated Total Abundance (N)",
    N_se    = "Estimated Total Abundance (N) with 95% CI",
    NA_     = "Estimated Abundance-at-Age",
    NA_se   = "Estimated Abundance-at-Age with 95% CI",
    NL      = "Estimated Abundance-at-Length",
    NL_se   = "Estimated Abundance-at-Length with 95% CI",

    # plot_biomass
    B       = "Estimated Total Biomass (B)",
    B_se    = "Estimated Total Biomass (B) with 95% CI",
    BL      = "Estimated Biomass-at-Length",

    # plot_SSB
    SSB     = "Estimated Spawning Stock Biomass (SSB)",
    SSB_se  = "Estimated Spawning Stock Biomass (SSB) with 95% CI",
    SBL     = "Estimated SSB-at-Length",

    # plot_recruitment
    Rec     = "Estimated Recruitment (R)",
    Rec_se  = "Estimated Recruitment (R) with 95% CI",

    # plot_SSB_Rec
    SSB_Rec = "Spawning Stock Biomass vs. Recruitment",

    # plot_catch
    CN      = "Estimated Catch Abundance (C)",
    CN_se   = "Estimated Catch Abundance (C) with 95% CI",
    CNA     = "Estimated Catch-at-Age",

    # plot_fishing_mortality
    F_year     = "Estimated Fishing Mortality by Year (F)",
    F_year_se  = "Estimated Fishing Mortality by Year (F) with 95% CI",
    F_age      = "Estimated Fishing Mortality-at-Age (F)",
    F_age_se   = "Estimated Fishing Mortality-at-Age (F) with 95% CI",

    # plot_deviance
    dev_R   = "Recruitment Deviance (dev log R)",
    dev_F   = "Fishing Mortality Deviance (dev log F)",

    # plot_VB
    VB      = "Von Bertalanffy Growth Curve",

    # plot_CatL
    CatL_obs_length = "Observed Catch-at-Length Over Years",
    CatL_est_length = "Estimated Catch-at-Length Over Years",
    CatL_year       = "Estimated(Line) and Observed(Point) Catch-at-Length Over Years",
    CatL_year_dist  = "Estimated(Red) and Observed(Blue) Catch-at-Length Distribution Yearly",

    # plot_residuals
    resid_length = "Residuals by Length Bins",
    resid_year   = "Residuals by Year",

    # plot_ridges
    ridges   = "Observed vs. Estimated Catch-at-Length",

    # retro_acl
    retro    = "Retrospective Analysis",

    # plot_pla
    pla      = "Age-Length Transition Probability Matrix"
  ),

  # --- Axis labels ---------------------------------------------------------
  xlab = list(
    year   = "Year",
    age    = "Age",
    length = "Length",
    ssb    = "SSB"
  ),

  ylab = list(
    abundance = "Relative abundance",
    biomass   = "Biomass",
    ssb       = "SSB",
    rec       = "Recruitment",
    catch     = "Catch abundance",
    F         = "Fishing mortality",
    deviance  = "Deviance",
    residual  = "Residual",
    growth    = "Length"
  )
)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Get Current ACL Plot Theme
#'
#' Returns the current global theme settings used by all ACL plot functions.
#'
#' @param what Optional character. Retrieve a specific element, e.g.
#'   \code{"titles"}, \code{"font_family"}, \code{"xlab"}.
#' @return A list of current settings, or a single element if \code{what}
#'   is specified.
#' @export
#' @examples
#' acl_theme()
#' acl_theme("font_family")
#' acl_theme("titles")$N
acl_theme <- function(what = NULL) {
  current <- getOption("acl.theme", .acl_defaults)
  if (is.null(what)) return(current)
  current[[what]]
}


#' Set ACL Plot Theme Options
#'
#' Modify one or more global theme settings. Partial updates are supported:
#' only the fields you specify will be changed; all others keep their
#' current values.
#'
#' @param base_theme Character. Base ggplot2 theme name. One of
#'   \code{"theme_bw"}, \code{"theme_minimal"}, \code{"theme_classic"},
#'   \code{"theme_gray"}, \code{"theme_light"}, \code{"theme_linedraw"},
#'   \code{"theme_void"}. Default is \code{"theme_bw"}.
#' @param font_family Character. Font family for all text elements.
#' @param title_size Numeric. Plot title size in pt.
#' @param title_hjust Numeric. Plot title horizontal alignment: 0 = left,
#'   0.5 = center (default), 1 = right.
#' @param axis_title_size Numeric. Axis title (x/y label) size in pt.
#' @param axis_text_size Numeric. Axis tick label size in pt.
#' @param strip_text_size Numeric. Facet panel label size in pt.
#' @param legend_text_size Numeric. Legend text size in pt.
#' @param x_breaks Numeric vector or NULL. Custom x-axis break points (e.g.
#'   \code{seq(1, 20, by = 2)} or \code{c(1, 5, 10, 15, 20)}).
#'   NULL = auto breaks.
#' @param x_expand Numeric vector of length 2. Expansion multiplier for x-axis,
#'   e.g. \code{c(0.01, 0.01)} for minimal padding. Default is \code{c(0.01, 0.01)}.
#' @param line_color Character. Default line color.
#' @param line_size Numeric. Default line width.
#' @param se_color Character. Default CI color.
#' @param se_alpha Numeric. Default CI transparency.
#' @param titles Named list. Override specific titles. Use
#'   \code{acl_theme("titles")} to see available keys.
#' @param xlab Named list. Override specific x-axis labels.
#' @param ylab Named list. Override specific y-axis labels.
#' @return Invisible previous settings.
#' @export
#' @examples
#' # Change base theme globally
#' acl_theme_set(base_theme = "theme_bw")          # default
#' acl_theme_set(base_theme = "theme_minimal")
#' acl_theme_set(base_theme = "theme_classic")
#'
#' # Change font globally
#' acl_theme_set(font_family = "Times New Roman")
#'
#' # Change text sizes
#' acl_theme_set(title_size = 16, axis_title_size = 14, axis_text_size = 12)
#'
#' # Change a specific title
#' acl_theme_set(titles = list(N = "Total Population Size"))
#'
#' # Change multiple settings at once
#' acl_theme_set(
#'   font_family     = "Helvetica",
#'   title_size      = 16,
#'   axis_title_size = 14,
#'   axis_text_size  = 11,
#'   strip_text_size = 11,
#'   line_color      = "steelblue",
#'   line_size       = 1.8,
#'   titles          = list(SSB = "Spawning Biomass", Rec = "Annual Recruitment")
#' )
acl_theme_set <- function(base_theme = NULL, font_family = NULL, title_size = NULL,
                          title_hjust = NULL,
                          axis_title_size = NULL, axis_text_size = NULL,
                          strip_text_size = NULL, legend_text_size = NULL,
                          x_breaks = NULL, x_expand = NULL,
                          line_color = NULL, line_size = NULL,
                          se_color = NULL, se_alpha = NULL,
                          titles = NULL, xlab = NULL, ylab = NULL) {

  current <- getOption("acl.theme", .acl_defaults)
  old <- current

  if (!is.null(base_theme))       current$base_theme       <- base_theme
  if (!is.null(font_family))      current$font_family      <- font_family
  if (!is.null(title_size))       current$title_size       <- title_size
  if (!is.null(title_hjust))      current$title_hjust      <- title_hjust
  if (!is.null(axis_title_size))  current$axis_title_size  <- axis_title_size
  if (!is.null(axis_text_size))   current$axis_text_size   <- axis_text_size
  if (!is.null(strip_text_size))  current$strip_text_size  <- strip_text_size
  if (!is.null(legend_text_size)) current$legend_text_size <- legend_text_size
  if (!is.null(x_breaks))        current$x_breaks        <- x_breaks
  if (!is.null(x_expand))        current$x_expand        <- x_expand
  if (!is.null(line_color))       current$line_color       <- line_color
  if (!is.null(line_size))        current$line_size        <- line_size
  if (!is.null(se_color))         current$se_color         <- se_color
  if (!is.null(se_alpha))         current$se_alpha         <- se_alpha
  if (!is.null(titles))           current$titles           <- modifyList(current$titles, titles)
  if (!is.null(xlab))             current$xlab             <- modifyList(current$xlab, xlab)
  if (!is.null(ylab))             current$ylab             <- modifyList(current$ylab, ylab)

  options(acl.theme = current)
  invisible(old)
}


#' Reset ACL Plot Theme to Defaults
#'
#' @export
#' @examples
#' acl_theme_reset()
acl_theme_reset <- function() {
  options(acl.theme = .acl_defaults)
  cat("ACL plot theme reset to defaults.\n")
}


# ---------------------------------------------------------------------------
# Internal helpers (used by plot functions)
# ---------------------------------------------------------------------------

#' Get a title from the theme
#' @param key Character. Title key (e.g. "N", "SSB_se").
#' @return Character string.
#' @keywords internal
.acl_title <- function(key) {
  result <- acl_theme("titles")[[key]]
  if (!is.null(result)) result else key
}

#' Get an axis label from the theme
#' @param axis Character. "x" or "y".
#' @param key Character. Label key (e.g. "year", "abundance").
#' @return Character string.
#' @keywords internal
.acl_lab <- function(axis = "x", key) {
  labs <- if (axis == "x") acl_theme("xlab") else acl_theme("ylab")
  result <- labs[[key]]
  if (!is.null(result)) result else key
}

#' Build the standard ACL ggplot theme layer
#' @param base_theme Optional base theme name override (e.g. "theme_bw").
#' @param font_family Optional font family override.
#' @param title_size Optional plot title size override.
#' @param axis_title_size Optional axis title size override.
#' @param axis_text_size Optional axis tick label size override.
#' @param strip_text_size Optional facet label size override.
#' @param legend_text_size Optional legend text size override.
#' @return A list of ggplot2 theme elements.
#' @keywords internal
.acl_base_theme <- function(font_family = NULL, title_size = NULL,
                            axis_title_size = NULL, axis_text_size = NULL,
                            strip_text_size = NULL, legend_text_size = NULL,
                            base_theme = NULL, title_hjust = NULL) {
  bt  <- if (!is.null(base_theme))        base_theme       else acl_theme("base_theme")
  ff  <- if (!is.null(font_family))       font_family      else acl_theme("font_family")
  ts  <- if (!is.null(title_size))        title_size       else acl_theme("title_size")
  th  <- if (!is.null(title_hjust))       title_hjust      else acl_theme("title_hjust")
  ats <- if (!is.null(axis_title_size))   axis_title_size  else acl_theme("axis_title_size")
  atx <- if (!is.null(axis_text_size))    axis_text_size   else acl_theme("axis_text_size")
  sts <- if (!is.null(strip_text_size))   strip_text_size  else acl_theme("strip_text_size")
  lts <- if (!is.null(legend_text_size))  legend_text_size else acl_theme("legend_text_size")

  # Resolve base theme function
  theme_fn <- switch(bt,
    "theme_bw"        = ggplot2::theme_bw,
    "theme_minimal"   = ggplot2::theme_minimal,
    "theme_classic"   = ggplot2::theme_classic,
    "theme_gray"      = ggplot2::theme_gray,
    "theme_grey"      = ggplot2::theme_grey,
    "theme_light"     = ggplot2::theme_light,
    "theme_linedraw"  = ggplot2::theme_linedraw,
    "theme_dark"      = ggplot2::theme_dark,
    "theme_void"      = ggplot2::theme_void,
    ggplot2::theme_bw  # fallback
  )

  list(
    theme_fn(),
    ggplot2::theme(
      text             = ggplot2::element_text(family = ff),
      plot.title       = ggplot2::element_text(size = ts, hjust = th),
      axis.title       = ggplot2::element_text(size = ats),
      axis.text        = ggplot2::element_text(size = atx),
      strip.text       = ggplot2::element_text(size = sts),
      legend.text      = ggplot2::element_text(size = lts),
      legend.title     = ggplot2::element_text(size = lts)
    )
  )
}



#' Build x-axis scale with breaks and expand
#'
#' @param x_breaks Numeric vector or NULL (override). If NULL, reads from
#'   global theme. If global is also NULL, uses \code{pretty_breaks(n)}.
#' @param n_breaks Integer. Fallback number of pretty breaks (default 10).
#' @return A ggplot2 scale layer.
#' @keywords internal
.acl_scale_x <- function(x_breaks = NULL, n_breaks = 10) {
  brks <- if (!is.null(x_breaks)) x_breaks else acl_theme("x_breaks")
  expd <- acl_theme("x_expand")
  if (is.null(expd)) expd <- c(0.01, 0.01)

  if (is.null(brks)) {
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = n_breaks),
      expand = ggplot2::expansion(mult = expd)
    )
  } else {
    ggplot2::scale_x_continuous(
      breaks = brks,
      expand = ggplot2::expansion(mult = expd)
    )
  }
}


#' Fix length bin labels: first/last bins become open-ended
#'
#' Converts "5-7" to "<7" and "49-51" to ">49" for the first and last bins.
#' Middle bins are kept as-is ("7-9", "9-11", ...).
#'
#' @param len_label Character vector of length bin labels.
#' @param prefix Character. Prefix for facet labels (e.g. "Length bin").
#' @return A character vector of fixed labels.
#' @keywords internal
.acl_fix_len_labels <- function(len_label, prefix = "Length bin") {
  LengthGroup <- len_label
  n_bins <- length(LengthGroup)
  if (n_bins >= 2) {
    first_parts <- strsplit(as.character(LengthGroup[1]), "-")[[1]]
    last_parts  <- strsplit(as.character(LengthGroup[n_bins]), "-")[[1]]
    if (length(first_parts) == 2) LengthGroup[1] <- paste0("<", first_parts[2])
    if (length(last_parts)  == 2) LengthGroup[n_bins] <- paste0(">", last_parts[1])
  }
  paste(prefix, LengthGroup)
}
