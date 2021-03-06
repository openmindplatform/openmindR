% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{om_mix_models}
\alias{om_mix_models}
\title{Run mixed effects model}
\usage{
om_mix_models(
  gathered_dat,
  question,
  plot_model = F,
  get_effects = F,
  get_tidy = F
)
}
\arguments{
\item{gathered_dat}{Assessment data as long format}

\item{question}{Specify question that you want to perform analysis for (ex: \code{"Q18"})}

\item{plot_model}{logical. Show a coefficient plot of the model. Default is \code{FALSE}}

\item{get_effects}{logical. Get marginal effects. Default is \code{FALSE}}

\item{get_tidy}{logical. Get a tidy dataframe with estimates. Also calculates pseudo cohen's d efecct sizes. Default is \code{FALSE}}
}
\description{
This function performs mixed models (Currently only works on Ann Miller experimental data)
}
