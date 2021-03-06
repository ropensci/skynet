% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PowerLaw.R
\name{fit_power}
\alias{fit_power}
\title{Power Law}
\usage{
fit_power(graph)
}
\arguments{
\item{graph}{iGraph object}
}
\description{
Plots power law fit
}
\examples{
\dontrun{
netDir <- make.netDir(OD_Sample)
fit_power(netDir$gDir)
}
}
