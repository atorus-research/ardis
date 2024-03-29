% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_summaries.R
\name{set_summaries}
\alias{set_summaries}
\alias{set_summaries.ardis_layer}
\title{Set Summaries}
\usage{
set_summaries(e, ...)

\method{set_summaries}{ardis_layer}(e, ...)
}
\arguments{
\item{e}{Layer upon which summaries should be bound}

\item{...}{Named parameters containing lists of quosures created using \code{dplyr::vars()}}
}
\value{
The layer environment with the summary variables binding applied
}
\description{
If ardis on only being used to summarize numeric data, then layer formatting
components of the settings become unnecessary. As such, a second interface is
necessary to specify which summaries should actually be performed. the
\code{set_summaries()} function provided an interface to each layer type to
establish the numbers that must be calculated.
}
\examples{
# This is the desired API for count layers
t <- ardis(adsl, TRT01P) \%>\%
  add_layer(
    group_desc(AGE, by = "Age (years)", where= SAFFL=="Y") \%>\%
      set_summaries(
        "n"        = vars(n),
        "Mean (SD)"= vars(mean, sd),
        "Median"   = vars(median),
        "Q1, Q3"   = vars(q1, q3),
        "Min, Max" = vars(min, max),
        "Missing"  = vars(missing)
      )
  )

}
