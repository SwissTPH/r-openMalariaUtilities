% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slurm_simulation.R
\name{slurmPrepareRunScenarios}
\alias{slurmPrepareRunScenarios}
\title{Run preparations for SLURM submission}
\usage{
slurmPrepareRunScenarios(
  expName,
  scenarios = NULL,
  ntasks = 1,
  memCPU = "250MB",
  time = "06:00:00",
  qos = "6hours",
  verbose = FALSE
)
}
\arguments{
\item{expName}{Name of experiment}

\item{scenarios}{Scenario data frame}

\item{ntasks}{Number of tasks per CPU}

\item{memCPU}{Memory per CPU}

\item{time}{Maximum time}

\item{qos}{Quality of service}

\item{verbose}{If TRUE, use OpenMalaria's verbose output.}
}
\description{
Run preparations for SLURM submission
}
