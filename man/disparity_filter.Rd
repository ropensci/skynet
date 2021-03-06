% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disparityfilter.R
\name{disparity_filter}
\alias{disparity_filter}
\title{Disparity Filter}
\usage{
disparity_filter(g, alpha = 0.003)
}
\arguments{
\item{g}{igraph object}

\item{alpha}{Alpha value. Default 0.003}
}
\description{
Uses the Serrano's disparity filter (\url{https://en.wikipedia.org/wiki/Disparity_filter_algorithm_of_weighted_network})
to extract the backbone of the network in "Extracting the multiscale backbone of complex weighted networks"
}
\examples{
\dontrun{
netDir <- make.netDir(OD_Sample)
disparity_filter(netDir$gDir, alpha = 0.003)
}
}
