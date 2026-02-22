#' @keywords internal
"_PACKAGE"

#' @importFrom stats arima.sim nlminb pnorm rnorm var
#' @importFrom scales pretty_breaks
#' @importFrom utils head modifyList tail write.csv
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon geom_tile
#'   geom_smooth geom_hline geom_text facet_wrap labs theme theme_minimal
#'   element_text scale_fill_gradient ggsave
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate group_by filter summarise ungroup first pull distinct %>%
#' @importFrom tidyr pivot_longer gather
#' @importFrom TMB compile MakeADFun sdreport
#' @importFrom RColorBrewer brewer.pal
NULL

# Suppress R CMD check notes for non-standard evaluation variables
utils::globalVariables(c(
 "Year", "Variable", "Value", "RetrospectiveYear", "Rho",
 "estimate", "lower", "upper",
 "Count", "Abundance", "LengthGroup", "AgeGroup",
 "number.age", "residual", "variable", "value",
 "SSB", "Rec", "age", "length_lower", "length_upper",
 "len_mid", "sim.data",
 "number", "biomass", "recruitment", "CN", "CNA", "F"
))
