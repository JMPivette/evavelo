% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_categ.R
\name{correct_spor_lois}
\alias{correct_spor_lois}
\title{Apply categorie_corrigee Methodology to decide between Sportif and Loisir}
\usage{
correct_spor_lois(data)
}
\arguments{
\item{data}{a data.frame}
}
\value{
a data.frame the same size of data with updated categorie_corrige values.
}
\description{
In Chapter 3.1.11, this corresponds to cases 6 11
}
\details{
this function can be used inside pipe operator and is compatible with dplyr
}
\keyword{internal}
