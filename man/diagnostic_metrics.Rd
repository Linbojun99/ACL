% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostic_metrics.R
\name{diagnostic_metrics}
\alias{diagnostic_metrics}
\title{Diagnostic function for ACL}
\usage{
diagnostic_metrics(data.CatL, model_result)
}
\arguments{
\item{data.CatL}{A matrix containing the length grouping of the catch length,
which represents the observed catch data in different length groups across years.}

\item{model_result}{A list that contains the model output. The list should have a "report" component which contains an "Elog_index" component representing counts in length groups.}
}
\value{
A list containing the calculated metrics: MSE, MAE, RMSE, Rsquared, MAPE, exp_var_score, max_error
}
\description{
Diagnostic function for ACL
}
\examples{
\dontrun{
diagnostic_metrics(data.CatL, model_result)
}
}
