% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_parameters.R
\name{create_parameters}
\alias{create_parameters}
\title{Create custom parameter bounds for the stock assessment model}
\usage{
create_parameters(parameters = NULL, parameters.L = NULL, parameters.U = NULL)
}
\arguments{
\item{parameters}{A list containing the custom initial values for the parameters (default is NULL).
\itemize{
\item log_init_Z: initial log(Z) value
\item log_std_log_N0: standard deviation of log(N0)
\item mean_log_R: mean of log(R)
\item log_std_log_R: standard deviation of log(R)
\item logit_log_R: logit of log(R)
\item mean_log_F: mean of log(F)
\item log_std_log_F: standard deviation of log(F)
\item logit_log_F_y: logit of log(F_y)
\item logit_log_F_a: logit of log(F_a)
\item log_vbk: log(vBk)
\item log_Linf: log(Linf)
\item t0: t0
\item log_cv_len: log(cv_len)
\item log_std_index: log(std_index)
}}

\item{parameters.L}{A list containing the custom lower bounds for the parameters (default is NULL).}

\item{parameters.U}{A list containing the custom upper bounds for the parameters (default is NULL).}
}
\value{
A list containing three elements:'parameters' 'parameters.L' and 'parameters.U', representing the generated lower and upper bounds for the parameters.
}
\description{
This function generates custom lower and upper bounds for the parameters of the stock assessment model.
Users can provide custom values for specific parameters, and the function will use the default values for the others.
}
