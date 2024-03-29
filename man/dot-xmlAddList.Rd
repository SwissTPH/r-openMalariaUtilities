% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xml.R
\name{.xmlAddList}
\alias{.xmlAddList}
\title{Add sublist to list}
\usage{
.xmlAddList(data, sublist, entry, input, append = TRUE)
}
\arguments{
\item{data}{List to add to.}

\item{sublist}{Sublist, under which the entry should get added. Must be
given as a vector, e.g. c("level1", "level2", ...). Can be NULL.}

\item{entry}{Name of the entry to add, as string. Can be NULL.}

\item{input}{List to add.}

\item{append}{Boolean. Determines if the list should be appended to existing
entries or overwrite them.}
}
\description{
Add sublist to list
}
\keyword{internal}
