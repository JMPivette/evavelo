% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_evavelo.R
\name{check_multiple_volume}
\alias{check_multiple_volume}
\title{Check volume abnomaly in multiple questionnaires}
\usage{
check_multiple_volume(eva_data)
}
\arguments{
\item{eva_data}{an eva_data object containing enquete and comptage}
}
\value{
a data.frame containing id_quest sum_taille unique_taille and volume_manuel
}
\description{
This function basically compares volume_manuel from comptage to taille_totale_groupe from enquete.
In case of multiple quest, volume_manuel should be the sum of taile_totale_groupe or its unique value.
All other possibilities are outputs from this function
}
\keyword{internal}
