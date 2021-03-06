% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{summarize_comparison}
\alias{summarize_comparison}
\title{Conducts t-tests and calculates Cohen's d}
\usage{
summarize_comparison(x, waves, q14_q17 = F)
}
\arguments{
\item{x}{data}

\item{waves}{either PrePost or PreFollow}

\item{q14_q17}{logical, Q14 and Q17 are coded seperately}
}
\description{
This is lower-level function that belongs to om_summarize_comparisons. This function is not meant to be used outside of om_summarize_comparisons.
}
