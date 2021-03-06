% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{df_compare}
\alias{df_compare}
\title{Compare 2 data.frames}
\usage{
df_compare(x, y, verbose = TRUE)
}
\arguments{
\item{x}{a data.frame}

\item{y}{a data.frame}

\item{verbose}{boolean to add warning when there is a mismatch}
}
\value{
a logical data.frame
}
\description{
This function compares 2 data.frames element_wise and returns a logical data.frame
NA values are also compared (as opposed to base R `==`)
Comparison is done columns by columns so both date.frames should be ordered.
}
\keyword{internal}
