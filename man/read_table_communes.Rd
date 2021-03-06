% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_evavelo.R
\name{read_table_communes}
\alias{read_table_communes}
\title{Read and clean "table_communes" information}
\usage{
read_table_communes(file, sheet = "table_communes")
}
\arguments{
\item{file}{xlsx file, Workbook object or URL to xlsx file}

\item{sheet}{Name of the worksheet containing "table_communes" information.}
}
\value{
a data.frame
}
\description{
Read a specific sheet of an xlsx object and perform some basic cleaning.
}
\keyword{internal}
