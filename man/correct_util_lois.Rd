% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_categ.R
\name{correct_util_lois}
\alias{correct_util_lois}
\title{Apply categorie_corrigee Methodology to decide between Utilitaire and Loisir}
\usage{
correct_util_lois(data)
}
\arguments{
\item{data}{a data.frame}
}
\value{
a data.frame the same size of data with updated categorie_corrige values.
}
\description{
In Chapter 3.1.11, this corresponds to cases 9 12
}
\details{
this function can be used inside pipe operator and is compatible with dplyr
}
\keyword{internal}
