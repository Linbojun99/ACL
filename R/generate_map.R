#' Generate a custom map or use default values for the stock assessment model
#'
#' This function generates a map for the stock assessment model with either custom or default values.
#' Users can provide custom values for specific map elements, and the function will use the default values for the others.
#'
#' @param map A list containing the custom values for the map elements (default is NULL).
#'   - log_std_log_F: Custom value for log_std_log_F (default is NA).
#'   - logit_log_F_y: Custom value for logit_log_F_y (default is NA).
#'   - logit_log_F_a: Custom value for logit_log_F_a (default is NA).
#'   - t0: Custom value for t0 (default is NA).
#'   - log_vbk: Custom value for log_vbk (default is NA).
#'   - log_Linf: Custom value for log_Linf (default is NA).
#'
#' @return A list representing the generated map with custom or default values.
#' @export
generate_map <- function(map = NULL) {
  default_map <- list(
    log_std_log_F = factor(NA),
    logit_log_F_y = factor(NA),
    logit_log_F_a = factor(NA),
    #log_vbk=factor(NA),
    #log_Linf=factor(NA),
    t0 = factor(NA)
  )

  if (!is.null(map)) {
    updated_map <- modifyList(default_map, map)
    return(updated_map[names(map)]) # Return only specified elements
  } else {
    return(default_map) # Return all elements with default values
  }
}
