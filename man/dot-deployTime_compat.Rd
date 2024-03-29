% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time.R
\name{.deployTime_compat}
\alias{.deployTime_compat}
\title{Basic function used for deploying interventions}
\usage{
.deployTime_compat(
  y1 = 2000,
  y2 = NULL,
  m1 = 5,
  m2 = NULL,
  d1 = 5,
  d2 = NULL,
  every = 1,
  interval = "year",
  ...
)
}
\arguments{
\item{y1}{Year of the first date (surveys starting from year y1)}

\item{y2}{Year of the end date (surveys continuing until year y2)}

\item{m1}{Month of the first date}

\item{m2}{Month of the end date}

\item{d1}{Day of the first date}

\item{d2}{Day of the end date}

\item{every}{Interval size}

\item{interval}{Interval size (days, weeks, )}

\item{...}{Deprecated arguments}
}
\description{
Compatibility with Munirflow deploy time format. Returns a list
with a startDate, endDate and interval for the new format.
}
