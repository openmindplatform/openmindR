% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning.R
\name{om_dummy_gender}
\alias{om_dummy_gender}
\title{Code a dummy variable for gender}
\usage{
om_dummy_gender(assessment)
}
\arguments{
\item{assessment}{assessment data}
}
\description{
This function creates two dummy variables from D2 (Gender) called \code{gender} and \code{gender_num} which codes people who identify as male as 0 and females else as 1.
}
\examples{
assessmentv7 \%>\%
  om_dummy_gender()
}
