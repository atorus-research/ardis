% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_utils.R
\name{get_meta_subset}
\alias{get_meta_subset}
\alias{get_meta_subset.data.frame}
\alias{get_meta_subset.ardis}
\title{Extract the subset of data based on result metadata}
\usage{
get_meta_subset(x, row_id, column, add_cols = vars(USUBJID), ...)

\method{get_meta_subset}{data.frame}(
  x,
  row_id,
  column,
  add_cols = vars(USUBJID),
  target = NULL,
  ...
)

\method{get_meta_subset}{ardis}(x, row_id, column, add_cols = vars(USUBJID), ...)
}
\arguments{
\item{x}{A built ardis table or a dataframe}

\item{row_id}{The row_id value of the desired cell, provided as a character
string}

\item{column}{The result column of interest, provided as a character string}

\item{add_cols}{Additional columns to include in subset data.frame output}

\item{...}{additional arguments}

\item{target}{A data frame to be subset (if not pulled from a ardis table)}
}
\value{
A data.frame
}
\description{
Given a row_id value and a result column, this function will return the
subset of data referenced by the ardis_meta object associated with that
'cell', which provides traceability to tie a result to its source.
}
\details{
If a ardis table is built with the \code{metadata=TRUE} option specified, then
metadata is assembled behind the scenes to provide traceability on each
result cell derived. The functions \code{get_meta_result()} and
\code{get_meta_subset()} allow you to access that metadata by using an ID provided
in the row_id column and the column name of the result you'd like to access.
The purpose is of the row_id variable instead of a simple row index is to
provide a sort resistant reference of the originating column, so the output
ardis table can be sorted in any order but the metadata are still easily
accessible.

The \code{ardis_meta} object provided a list with two elements - names and
filters. The metadata contain every column from the target data.frame of the
ardis table that factored into the specified result cell, and the filters
contains all the necessary filters to subset to data summarized to create the
specified result cell. \code{get_meta_subset()} additionally provides a parameter
to specify any additional columns you would like to include in the returned
subset data frame.
}
\examples{
t <- ardis(mtcars, cyl) \%>\%
  add_layer(
    group_desc(hp)
  )


dat <- t \%>\% build(metadata = TRUE)

get_meta_subset(t, 'd1_1', 'var1_4', add_cols = dplyr::vars(carb))

m <- t$metadata
dat <- t$target

get_meta_subset(t, 'd1_1', 'var1_4', add_cols = dplyr::vars(carb), target = target)
}
