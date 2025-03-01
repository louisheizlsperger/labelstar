% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_labels.R
\name{get_labels}
\alias{get_labels}
\title{Retrieve Labels for Outcome Variable, Covariates, Fixed Effects, and Clustering Variables from Formulas}
\usage{
get_labels(
  formulas,
  data,
  omit_vars = character(0),
  interaction_symbol = " : ",
  fe_symbol = "X",
  dep_var_means = "no"
)
}
\arguments{
\item{formulas}{A list or vector of model formulas.}

\item{data}{The data frame containing the variables.}

\item{omit_vars}{A vector of strings specifying variable names whose labels should be omitted from covariate_labels.}

\item{interaction_symbol}{A character string representing the preferred symbol for interaction terms (e.g., ":" or "x").}

\item{fe_symbol}{A character string representing the preferred symbol for the inclusion of fixed effects (e.g., "x" or "Yes").}

\item{dep_var_means}{Either "no", "raw", or "transformed". If not "no", the mean of each dependent variable is computed.}
}
\value{
A list with elements: \code{dep_var_labels} (a vector of outcome labels),
\code{covariate_labels}, \code{add_lines} (which now includes the row with dependent variable means), and \code{table_notes}.
}
\description{
Now accommodates a regression list with potentially different outcome variables.
}
