% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xml_interventions_define.R
\name{defineIRS}
\alias{defineIRS}
\alias{define_IRS}
\title{Adds the IRS intervention parameterisation}
\usage{
defineIRS(
  baseList,
  mosquitos,
  component = c("Actellic50EC", "Actellic300CS", "Actellic11", "Actellic8", "Actellic80",
    "magic_actellic", "IRS4", "IRS6", "IRS8", "Bendiocarb", "nothing"),
  noeffect = "outdoor",
  steplife = NULL
)

define_IRS(
  baseList,
  mosquitos,
  component = c("Actellic50EC", "Actellic300CS", "Actellic11", "Actellic8", "Actellic80",
    "magic_actellic", "IRS4", "IRS6", "IRS8", "Bendiocarb", "nothing"),
  noeffect = "outdoor",
  steplife = NULL
)
}
\arguments{
\item{baseList}{List with experiment data.}

\item{mosquitos}{Mosquito species affected by the intervention.}

\item{component}{Insecticde to use. This will automatically specify efficacy
and effect duration. Needs to from predefined choices.}

\item{noeffect}{Pattern which defines which mosquitoes are unaffected by
intervention. Can be "outdoor", "indoor" or NULL.}

\item{steplife}{Step function decay, the number of months it's effective.}
}
\description{
Adds the IRS intervention parameterisation
}
