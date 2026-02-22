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

  # --- Font ----------------------------------------------------------------
  font_family = "Arial",

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
#' @param font_family Character. Font family for all text elements.
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
#' # Change font globally
#' acl_theme_set(font_family = "Times New Roman")
#'
#' # Change a specific title
#' acl_theme_set(titles = list(N = "Total Population Size"))
#'
#' # Change axis label
#' acl_theme_set(ylab = list(abundance = "Population (millions)"))
#'
#' # Change multiple settings at once
#' acl_theme_set(
#'   font_family = "Helvetica",
#'   line_color  = "steelblue",
#'   line_size   = 1.8,
#'   titles      = list(SSB = "Spawning Biomass", Rec = "Annual Recruitment")
#' )
acl_theme_set <- function(font_family = NULL, line_color = NULL, line_size = NULL,
                          se_color = NULL, se_alpha = NULL,
                          titles = NULL, xlab = NULL, ylab = NULL) {

  current <- getOption("acl.theme", .acl_defaults)
  old <- current

  if (!is.null(font_family)) current$font_family <- font_family
  if (!is.null(line_color))  current$line_color  <- line_color
  if (!is.null(line_size))   current$line_size   <- line_size
  if (!is.null(se_color))    current$se_color    <- se_color
  if (!is.null(se_alpha))    current$se_alpha    <- se_alpha
  if (!is.null(titles))      current$titles      <- modifyList(current$titles, titles)
  if (!is.null(xlab))        current$xlab        <- modifyList(current$xlab, xlab)
  if (!is.null(ylab))        current$ylab        <- modifyList(current$ylab, ylab)

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
#' @return A list of ggplot2 theme elements.
#' @keywords internal
.acl_base_theme <- function() {
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(text = ggplot2::element_text(family = acl_theme("font_family")))
  )
}


