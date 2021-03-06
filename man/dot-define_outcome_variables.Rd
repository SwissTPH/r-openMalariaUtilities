% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postprocess-compat.R
\name{.define_outcome_variables}
\alias{.define_outcome_variables}
\title{Define outcome variables}
\usage{
.define_outcome_variables(
  rawdat,
  units_of = 1000,
  age_variables = c("age_group", "age_category"),
  model_variables = c("models", "seed", "scenario"),
  time_variables = c("survey", "Timestep", "Date", "year"),
  outVars = selectedOutVars
)
}
\arguments{
\item{rawdat}{dataframe of raw simulation outputs}

\item{units_of}{integer value (e.g. 1e3 for units of 1000s )}

\item{age_variables}{names of the variables that denote age in the population, usually includes "age_group","age_category"}

\item{model_variables}{names of the basic model variables, usually includes "models","seed","scenario"}

\item{time_variables}{names of the variables that denote time, usually includes "survey","Timestep","Date","year"}

\item{outVars}{names of the outcome variables}
}
\description{
Define outcome variables
}
\note{
used often internally, never seen by user, medium to write, never breaks
}
