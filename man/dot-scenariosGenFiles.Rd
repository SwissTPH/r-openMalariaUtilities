% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scenarios.R
\name{.scenariosGenFiles}
\alias{.scenariosGenFiles}
\title{Generate scenario xml files}
\usage{
.scenariosGenFiles(
  scenarios,
  baseFile,
  range,
  placeholders,
  prefix,
  ncores = 1
)
}
\arguments{
\item{scenarios}{Data frame containing the values for the placeholders. One
row per scenario, placeholders in columns. Column names correspond to the
placeholder names.}

\item{baseFile}{Compatible base xml file.}

\item{range}{Row range of scenarios}

\item{placeholders}{Vector containing the placeholders}

\item{prefix}{Filename prefix}

\item{ncores}{Number of parallel processes to use.}
}
\description{
Generate scenario xml files
}
\keyword{internal}
