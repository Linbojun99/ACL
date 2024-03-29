% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_map.R
\name{generate_map}
\alias{generate_map}
\title{Generate a custom map or use default values for the stock assessment model}
\usage{
generate_map(map = NULL)
}
\arguments{
\item{map}{A list containing the custom values for the map elements (default is NULL).
\itemize{
\item log_std_log_F: Custom value for log_std_log_F (default is NA).
\item logit_log_F_y: Custom value for logit_log_F_y (default is NA).
\item logit_log_F_a: Custom value for logit_log_F_a (default is NA).
\item t0: Custom value for t0 (default is NA).
\item log_vbk: Custom value for log_vbk (default is NA).
\item log_Linf: Custom value for log_Linf (default is NA).
}}
}
\value{
A list representing the generated map with custom or default values.
}
\description{
This function generates a map for the stock assessment model with either custom or default values.
Users can provide custom values for specific map elements, and the function will use the default values for the others.
}
