% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{radical_quest}
\alias{radical_quest}
\title{Remove the multiple information from id_quest}
\usage{
radical_quest(id_quest)
}
\arguments{
\item{id_quest}{a character vector of id_quest to modify}
}
\value{
a character vector the same size of `id_quest` without suffix
}
\description{
In a case of multiple id_quest for the same groups, they wil be named "ABC-01", "ABC-02", etc..
This function removes the suffix and keep only "ABC" in this example.
}
\keyword{internal}
