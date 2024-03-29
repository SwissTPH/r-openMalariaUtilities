% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scenarios.R
\name{setupScenarios}
\alias{setupScenarios}
\alias{setup_scenarios}
\title{Generate scenarios from a base xml file}
\usage{
setupScenarios(
  baseFile = NULL,
  prefix = NULL,
  scenarios,
  ncores = 1,
  rowStart = NULL,
  rowEnd = NULL
)

setup_scenarios(
  baseFile = NULL,
  prefix = NULL,
  scenarios,
  ncores = 1,
  rowStart = NULL,
  rowEnd = NULL
)
}
\arguments{
\item{baseFile}{Compatible base xml file.}

\item{prefix}{Prefix for the scenario files.}

\item{scenarios}{Data frame containing the values for the placeholders. One
row per scenario, placeholders in columns. Column names correspond to the
placeholder names.}

\item{ncores}{Number of parallel processes to use.}

\item{rowStart}{Starting row. Optional.}

\item{rowEnd}{End row. Optional.}
}
\description{
Function generates scenarios defined in a data frame. In this
data frame each row is a scenario, placeholder values are in the columns.
Column names correspond to the placeholder names.
}
